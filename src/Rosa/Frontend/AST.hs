{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rosa.Frontend.AST (
  ExternDecl(..),
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

data ExternDecl                                             -- external declaration, i.e. declaration outside of a function
  = FuncDecl Ident [Ident]                                  -- function declaration, e.g., `int fun();`
  | FuncDefn Ident [Ident] [BlockItem]                      -- function definition, e.g., `int fun() { return 0; }`
  deriving (Eq, Show, Generic)

instance Monad m => Serial m ExternDecl

data BlockItem
  = VarDecl Ident                                           -- variable declaration, e.g., `int x;`
  | VarDefn Ident Expr                                      -- variable definition, e.g. `int x = 1;`
  | BlockStmt Stmt
  deriving (Eq, Show, Generic)

instance Monad m => Serial m BlockItem

data Stmt
  = ExprStmt (Maybe Expr)
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
  = NumLit Word64 -- TODO: replace with String or Text
  --StrLit String -- FIXME: Comming soon!
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
