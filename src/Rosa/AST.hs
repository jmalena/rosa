module Rosa.AST (
  Defn(..),
  Stmt(..),
  Expr(..),
  Lit(..),
  UnaryOp(..),
  BinaryOp(..)
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
  | BinaryOp BinaryOp Expr Expr
  deriving (Eq, Show)

data Lit
  = LInt64 Int
  deriving (Eq, Show)

data UnaryOp
  = OpBitCompl
  | OpLogCompl
  | OpAddCompl
  deriving (Eq, Show)

data BinaryOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  deriving (Eq, Show)
