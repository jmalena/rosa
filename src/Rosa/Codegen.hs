{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rosa.Codegen (
  runCodegen,
  codegen
) where

import Control.Monad.State

import Rosa.AST

data CodegenState = CodegenState
  { asm :: String
  } deriving (Show)

newtype Codegen a = Codegen { runCodegen' :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

runCodegen :: Codegen a -> String
runCodegen = asm . flip execState codegenState . runCodegen'
  where codegenState = CodegenState { asm = "" }

emit :: Int -> String -> Codegen ()
emit indent instr = do
  let s = replicate indent ' ' <> instr
  modify $ \rec -> rec { asm = asm rec <> s <> "\n" }

--------------------------------------------------------------------------------

codegen :: Defn -> Codegen ()
codegen defn = do
  emit 0 $ ".global _main"
  emit 0 ""
  emitDefn defn

emitDefn :: Defn -> Codegen ()
emitDefn (Func ident body) = do
  emit 0 $ "_" <> ident <> ":"
  mapM_ emitStmt body
  emit 2 "ret"
  emit 0 ""

emitStmt :: Stmt -> Codegen ()
emitStmt (Return expr) =
  emitExpr "rax" expr

emitExpr :: String -> Expr -> Codegen ()
emitExpr reg (Lit (LInt64 val)) =
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
emitExpr reg (BinaryOp OpAdd expr1 expr2) = do
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx" -- make %rcx variable
  emit 2 $ "addq %rcx, %" <> reg -- make %rcx variable
emitExpr reg (BinaryOp OpSub expr1 expr2) = do
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx" -- make %rcx variable
  emit 2 $ "subq %" <> reg <> ", %rcx" -- make %rcx variable
  emit 2 $ "movq %rcx, %" <> reg -- probably inefficient due to this redundant (?) movq
emitExpr reg (BinaryOp OpMul expr1 expr2) =
  error "64-bit multiplication is not supported yet"
emitExpr reg (BinaryOp OpDiv expr1 expr2) =
  error "64-bit division is not supported yet"
