{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Expr where

import {-# SOURCE #-} Language.Rosa.Ast.Pattern
import Language.Rosa.Core

-- | NOTE: PatternF fixpoint is defined here.
type Pattern p = Fix (PatternF p)

type family XVar      p
type family XApp      p
type family XAbs      p
type family XPi       p
type family XUniverse p
type family XTy       p
type family XLet      p
type family XBoolLit  p
type family XIntLit   p

data ExprF p f
  = Var (XVar p) String           -- ^ variable
  | App (XApp p) f f              -- ^ argument
  | ImpApp (XApp p) f             -- ^ {argument}
  | Abs (XAbs p) (Pattern p) f    -- ^ λ-abstraction
  | Pi (XPi p) (Pattern p) f f    -- ^ Π-type
  | Universe (XUniverse p) Word   -- ^ universe type
  | Ty (XTy p) f f                -- ^ type ascription
  | Let (XLet p) (Pattern p) f f  -- ^ let-binding
  | BoolLit (XBoolLit p) Bool     -- ^ boolean literal
  | IntLit (XIntLit p) Word       -- ^ integer literal
  deriving Functor

type Expr p = Fix (ExprF p)
