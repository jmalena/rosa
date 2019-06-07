module Rosa.AST (
  Defn(..),
  Stmt(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..)
) where

import Data.Word

data Defn
  = Func String [Stmt]
  deriving (Eq, Show)

data Stmt
  = Decl String (Maybe Expr)
  | SideEff Expr
  | Return Expr
  deriving (Eq, Show)

data Expr
  = Lit64 Word64 -- keep only non-negative numbers literals due to double representation of negative numbers: (Lit64 -1) vs. (UnaryOp OpBitCompl (Lit64 1)).
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
