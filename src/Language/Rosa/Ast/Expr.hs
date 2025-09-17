{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Expr where

import Language.Rosa.Ast.Pattern

type family XVar      p
type family XApp      p
type family XAbs      p
type family XPi       p
type family XUniverse p
type family XTyped    p
type family XLet      p
type family XBoolLit  p
type family XIntLit   p

data Expr p
  = Var (XVar p) String                        -- ^ variable reference
  | App (XApp p) (Expr p) (Expr p)             -- ^ function application
  | Abs (XAbs p) (Pattern p) (Expr p)          -- ^ λ-abstraction
  | Pi (XPi p) (Pattern p) (Expr p) (Expr p)   -- ^ Π-type
  | Universe (XUniverse p) Word                -- ^ universe type
  | Typed (XTyped p) (Expr p) (Expr p)         -- ^ type ascription
  | Let (XLet p) (Pattern p) (Expr p) (Expr p) -- ^ let-binding
  | BoolLit (XBoolLit p) Bool                  -- ^ boolean literal
  | IntLit (XIntLit p) Word                    -- ^ integer literal
