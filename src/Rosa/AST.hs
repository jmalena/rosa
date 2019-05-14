module Rosa.AST (
  Defn(..),
  Stmt(..),
  Expr(..),
  Lit(..),
  UnaryOp(..)
) where

data Defn
  = Func String [Stmt]
  deriving (Eq, Show)

data Stmt
  = Return Expr
  deriving (Eq, Show)

data Expr
  = Lit Lit
  | UnaryOp UnaryOp Expr
  deriving (Eq, Show)

data Lit
  = LInt Int
  deriving (Eq, Show)

data UnaryOp
  = BitCompl
  | LogCompl
  | Negation
  deriving (Eq, Show)
