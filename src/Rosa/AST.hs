module Rosa.AST (
  Defn(..),
  Stmt(..),
  Expr(..),
  Lit(..)
) where

data Defn
  = Func String [Stmt]
  deriving (Eq, Show)

data Stmt
  = Return Expr
  deriving (Eq, Show)

data Expr
  = Lit Lit
  deriving (Eq, Show)

data Lit
  = LInt Int
  deriving (Eq, Show)
