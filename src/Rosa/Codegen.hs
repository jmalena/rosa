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
  emit 0 $ ident <> ":"
  mapM_ emitStmt body
  emit 2 "ret"
  emit 0 ""

emitStmt :: Stmt -> Codegen ()
emitStmt (Return (Lit (LInt val))) =
  emit 2 $ "movl $" <> show val <> ", %eax"
