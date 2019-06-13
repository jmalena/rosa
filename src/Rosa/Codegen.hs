{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rosa.Codegen (
  runCodegen,
  codegen
) where

import Control.Monad.State

import Data.List
import qualified Data.Map as Map

import Debug.Trace

import Rosa.AST

data CodegenState = CodegenState
  { asm :: String
  , labelCounter :: Int
  , scopeStack :: [Scope]
  } deriving (Show)

data Scope = Scope
  { scopeVariables :: Map.Map String Int
  , scopeSize :: Int
  } deriving (Eq, Show)

newtype Codegen a = Codegen { runCodegen' :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

runCodegen :: Codegen a -> String
runCodegen = asm . flip execState codegenState . runCodegen'
  where codegenState = CodegenState { asm = "", labelCounter = 0, scopeStack = [] }

emit :: Int -> String -> Codegen ()
emit indent instr = do
  let s = replicate indent ' ' <> instr
  modify $ \rec -> rec { asm = asm rec <> s <> "\n" }

genLabel :: Codegen String
genLabel = do
  n <- gets labelCounter
  let name = "_clause_" <> show n
  modify $ \rec -> rec { labelCounter = succ n }
  return name

pushScope :: Codegen ()
pushScope =
  modify $ \rec@(CodegenState { scopeStack }) ->
    rec { scopeStack = (Scope { scopeVariables = Map.empty, scopeSize = 8 }):scopeStack }

popScope :: Codegen ()
popScope =
  modify $ \rec@(CodegenState { scopeStack = (_:scopeStack') }) ->
    rec { scopeStack = scopeStack' }

addScopeVar64 :: String -> Codegen ()
addScopeVar64 ident =
  modify $ \rec@(CodegenState { scopeStack = scope@(Scope { scopeVariables, scopeSize }):scopeStack }) ->
    let
      scope' = scope { scopeVariables = Map.insert ident scopeSize scopeVariables
                     , scopeSize = scopeSize+8
                     }
    in
      rec { scopeStack = scope':scopeStack }

findVarsScope :: String -> Codegen (Maybe Scope)
findVarsScope ident =
  find (\(Scope { scopeVariables }) -> ident `Map.member` scopeVariables) <$> gets scopeStack

getVarStackPointerOffset :: String -> Codegen Int
getVarStackPointerOffset ident = do
  identScopeM <- findVarsScope ident
  case identScopeM of
    Nothing -> error $ "Error: Variable " <> ident <> " is not defined"
    Just identScope@(Scope { scopeVariables }) -> do
      precedingScopes <- takeWhile (/= identScope) <$> gets scopeStack
      let skipSize = sum (map scopeSize precedingScopes)
      let scopeOffset = scopeVariables Map.! ident
      return $ skipSize + (scopeSize identScope - scopeOffset - 8) -- subtract 8 to skip saved return address

dumpScopes :: Codegen ()
dumpScopes =
  gets scopeStack >>= traceShowM

--------------------------------------------------------------------------------

codegen :: [Defn] -> Codegen ()
codegen defns = do
  emit 0 $ ".global _main"
  emit 0 ""
  mapM_ emitDefn defns

emitDefn :: Defn -> Codegen ()
emitDefn (Func ident body) = do
  emit 0 $ "_" <> ident <> ":"
  pushScope
  emit 2 $ "pushq %rbp"
  emit 2 $ "movq %rsp, %rbp"
  mapM_ emitStmt body
  emit 2 $ "movq %rbp, %rsp"
  emit 2 $ "popq %rbp"
  popScope
  emit 2 "ret"
  emit 0 ""

emitStmt :: Stmt -> Codegen ()
emitStmt (Decl ident Nothing) = do
  emit 2 "pushq $0"
  addScopeVar64 ident
emitStmt (Decl ident (Just expr)) = do
  emitExpr "rax" expr
  emit 2 "pushq %rax"
  addScopeVar64 ident
emitStmt (SideEff expr) =
  emitExpr "rax" expr
emitStmt (Return expr) =
  emitExpr "rax" expr

emitExpr :: String -> Expr -> Codegen ()
emitExpr reg (Lit64 val) =
  emit 2 $ "movq $" <> show val <> ", %" <> reg
emitExpr reg (UnaryOp OpBitCompl expr) = do
  emitExpr reg expr
  emit 2 $ "not %" <> reg
emitExpr reg (UnaryOp OpLogCompl expr) = do
  emitExpr reg expr
  emit 2 $ "cmpq $0, %" <> reg
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "sete %al"
emitExpr reg (UnaryOp OpAddCompl expr) = do
  emitExpr reg expr
  emit 2 $ "neg %" <> reg
emitExpr reg (BinaryOp OpMul expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "pushq %rdx"
  emit 2 $ "mulq %rcx" -- modifies %rax (result low) and %rdx (result high)
  emit 2 $ "popq %rdx"
  emit 2 $ "popq %rcx"
{-
emitExpr reg (BinaryOp OpDiv expr1 expr2) = do
  see https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf
-}
emitExpr reg (BinaryOp OpAdd expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "addq %rcx, %" <> reg
  emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpSub expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "subq %" <> reg <> ", %rcx"
  emit 2 $ "movq %rcx, %" <> reg
  emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpEQ expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "sete %al"
  emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpNEQ expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setne %al"
  emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpLTE expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setle %al"
  emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpLT expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setl %al"
  emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpGTE expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setge %al"
  emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpGT expr1 expr2) = do
  emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setg %al"
  emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpLogAnd expr1 expr2) = do
  emitExpr reg expr1
  emit 2 $ "cmpq $0, %" <> reg
  label1 <- genLabel
  emit 2 $ "jne " <> label1
  label2 <- genLabel
  emit 2 $ "jmp " <> label2
  emit 0 $ label1 <> ":"
  emitExpr reg expr2
  emit 2 $ "cmpq $0, %" <> reg
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setne %al"
  emit 0 $ label2 <> ":"
emitExpr reg (BinaryOp OpLogOr expr1 expr2) = do
  emitExpr reg expr1
  emit 2 $ "cmpq $0, %" <> reg
  label1 <- genLabel
  emit 2 $ "je " <> label1
  emit 2 $ "movq $1, %" <> reg
  label2 <- genLabel
  emit 2 $ "jmp " <> label2
  emit 0 $ label1 <> ":"
  emitExpr reg expr2
  emit 2 $ "cmpq $0, %" <> reg
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setne %al"
  emit 0 $ label2 <> ":"
emitExpr reg (Assign ident expr) = do
  memoryOffset <- getVarStackPointerOffset ident
  emitExpr reg expr
  emit 2 $ "movq %" <> reg <> ", " <> show memoryOffset <> "(%rsp)"
emitExpr reg (Ref ident) = do
  memoryOffset <- getVarStackPointerOffset ident
  emit 2 $ "movq " <> show memoryOffset <> "(%rsp), %" <> reg
