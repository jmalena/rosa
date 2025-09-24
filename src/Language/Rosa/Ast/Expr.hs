{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Expr where

import {-# SOURCE #-} Language.Rosa.Ast.Pattern
import Language.Rosa.Core

-- | Notice `Pattern` is defined here.
type Pattern p = Fix (PatternF p)

type family XUni     p
type family XBoolLit p
type family XIntLit  p
type family XVar     p
type family XImp     p
type family XAsc     p
type family XApp     p
type family XAbs     p
type family XPi      p
type family XLet     p

data ExprF p f
  = Uni (XUni p) Word             -- ^ universe type
  | BoolLit (XBoolLit p) Bool     -- ^ boolean literal
  | IntLit (XIntLit p) Word       -- ^ integer literal
  | Var (XVar p) String           -- ^ variable
  | Imp (XImp p) f                -- ^ implicit "{expr}"
  | Asc (XAsc p) f f              -- ^ (type) ascription
  | App (XApp p) f f              -- ^ application
  | Abs (XAbs p) (Pattern p) f    -- ^ λ-abstraction
  | Pi (XPi p) (Pattern p) f f    -- ^ Π-type
  | Let (XLet p) (Pattern p) f f  -- ^ let-binding
  deriving Functor

type Expr p = Fix (ExprF p)
