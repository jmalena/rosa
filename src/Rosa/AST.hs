module Rosa.AST (
  Defn(..),
  Stmt(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..),
  precOver
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

--------------------------------------------------------------------------------
-- | Precedence

class Precedence a where
  prec :: a -> Int

instance Precedence UnaryOp where
  prec OpBitCompl = 2
  prec OpLogCompl = 2
  prec OpAddCompl = 2

instance Precedence BinaryOp where
  prec OpMul = 3
  prec OpDiv = 3
  prec OpAdd = 4
  prec OpSub = 4
  prec OpLTE = 6
  prec OpLT = 6
  prec OpGTE = 6
  prec OpGT = 6
  prec OpEQ = 7
  prec OpNEQ = 7
  prec OpLogAnd = 11
  prec OpLogOr = 12

precOver :: (Precedence a, Precedence b)
         => a
         -> b
         -> Bool
precOver a b = prec a > prec b
