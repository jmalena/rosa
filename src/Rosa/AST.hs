{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rosa.AST (
  Defn(..),
  BlockItem(..),
  Stmt(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..),
  Ident,
  mkIdent,
  getRawIdent,
  precOver
) where

import Data.Char
import Data.List
import Data.Word

import GHC.Generics (Generic)

import Test.SmallCheck.Series

data Defn
  = FuncDecl Ident [Ident] (Maybe [BlockItem])
  deriving (Eq, Show, Generic)

instance Monad m => Serial m Defn

data BlockItem
  = BlockDecl Ident (Maybe Expr)
  | BlockStmt Stmt
  deriving (Eq, Show, Generic)

instance Monad m => Serial m BlockItem

data Stmt
  = SideEff (Maybe Expr)
  | Compound [BlockItem]
  | If Expr Stmt (Maybe Stmt)
  | For (Maybe Expr) (Maybe Expr) (Maybe Expr) Stmt
  | While Expr Stmt
  | Do Stmt Expr
  | Break
  | Continue
  | Return Expr
  deriving (Eq, Show, Generic)

instance Monad m => Serial m Stmt

data Expr
  = Lit64 Word64 -- keep only non-negative numbers literals due to double representation of negative numbers: (Lit64 -1) vs. (UnaryOp OpBitCompl (Lit64 1)).
  | Ref Ident
  | Assign Ident Expr
  | FuncCall Ident [Expr]
  | UnaryOp UnaryOp Expr
  | BinaryOp BinaryOp Expr Expr
  deriving (Eq, Show, Generic)

instance Monad m => Serial m Expr

data UnaryOp
  = OpBitCompl
  | OpLogCompl
  | OpAddCompl
  deriving (Eq, Show, Generic)

instance Monad m => Serial m UnaryOp

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
  deriving (Eq, Show, Generic)

instance Monad m => Serial m BinaryOp

--------------------------------------------------------------------------------
-- | Ident

newtype Ident = Ident { getRawIdent :: String }
  deriving (Eq, Ord, Show)

instance Monad m => Serial m Ident where
  series = generate $ \d -> [Ident "a"]

mkIdent :: String -> Maybe Ident
mkIdent "" = Nothing
mkIdent s = if isValid s then Just (Ident s) else Nothing
  where
    isValid (x:xs) =
      (isAlpha x || x == '_')
      && all (\x -> isAlpha x || isDigit x || x == '_') xs

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
