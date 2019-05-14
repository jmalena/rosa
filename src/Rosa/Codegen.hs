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
emitStmt (Return expr) = do
  case extractLit expr of
    LInt val ->
      emit 2 $ "movl $" <> show val <> ", %eax"
  emitExpr "eax" expr

emitExpr :: String -> Expr -> Codegen ()
emitExpr _ (Lit _) = pure ()
emitExpr reg (UnaryOp BitCompl _) =
  emit 2 $ "not %" <> reg
emitExpr reg (UnaryOp LogCompl _) = do
  emit 2 $ "cmpl $0, %" <> reg
  emit 2 $ "movl $0, %" <> reg
  emit 2 $ "sete %al"
emitExpr reg (UnaryOp Negation _) =
  emit 2 $ "neg %" <> reg

extractLit :: Expr -> Lit
extractLit (Lit lit) = lit
extractLit (UnaryOp _ expr) = extractLit expr
