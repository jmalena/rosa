module Language.Rosa.Syntax.Operators (
  UnaryOp(..),
  BinaryOp(..),
  Precedence(..)
) where

-- | Unary operators
data UnaryOp
  = OpPlus     -- prefix plus
  | OpMin      -- prefix minus
  | OpNeg      -- logical negation

-- | Binary operators
data BinaryOp
  = OpMul      -- multiplication
  | OpDiv      -- division
  | OpAdd      -- addition
  | OpSub      -- subtraction
  | OpLt       -- less than
  | OpGt       -- greater than
  | OpLte      -- less than or equal to
  | OpGte      -- greater than or equal to
  | OpEq       -- equal to
  | OpNeq      -- not equal to
  | OpAnd      -- logical AND
  | OpOr       -- logical OR

-- | Operator precedence
class Precedence a where
  prec :: a -> Int
  
  -- | Returns True if this operator has higher precedence than another
  precOver :: Precedence b => a -> b -> Bool
  precOver a b = prec a > prec b

instance Precedence UnaryOp where
  prec OpPlus = 2
  prec OpMin  = 2
  prec OpNeg  = 2

instance Precedence BinaryOp where
  prec OpMul = 3
  prec OpDiv = 3
  prec OpAdd = 4
  prec OpSub = 4
  prec OpLt  = 6
  prec OpGt  = 6
  prec OpLte = 6
  prec OpGte = 6
  prec OpEq  = 7
  prec OpNeq = 7
  prec OpAnd = 11
  prec OpOr  = 12
