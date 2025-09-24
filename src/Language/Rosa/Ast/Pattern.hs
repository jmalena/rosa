{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Pattern where

import Language.Rosa.Ast.Expr

type family XPWildcard p
type family XPInt      p
type family XPCon      p
type family XPVar      p

data PatternF p f
  = PWildcard (XPWildcard p)                  -- ^ wildcard "_"
  | PCon (XPCon p) String [f]                 -- ^ data constructor
  | PInt (XPInt p) Word                       -- ^ integer literal
  | PVar (XPVar p) String                     -- ^ variable
  | PVarImp (XPVar p) String (Maybe (Expr p)) -- ^ implicit variable
  deriving Functor
