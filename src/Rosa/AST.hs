module Rosa.AST (
  Defn(..),
  Stmt(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..)
) where

data Defn
  = Func String [Stmt]
  deriving (Eq, Show)

data Stmt
  = Decl String (Maybe Expr)
  | SideEff Expr
  | Return Expr
  deriving (Eq, Show)

data Expr
  = LInt64 Int
  | Ref String
  | Assign String Expr
  | UnaryOp UnaryOp Expr
  | BinaryOp BinaryOp Expr Expr
  deriving (Eq, Show)

data UnaryOp
  = OpBitCompl
  | OpLogCompl
  | OpAddCompl
  deriving (Eq, Show)

data BinaryOp
  = OpMul
  | OpDiv
  | OpAdd
  | OpSub
  | OpLTE
  | OpLT
  | OpGTE
  | OpGT
  | OpEQ
  | OpNEQ
  | OpLogAnd
  | OpLogOr
  deriving (Eq, Show)
