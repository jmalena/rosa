{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Pattern where

import Language.Rosa.Ast.Expr

type family XPCon      p
type family XPVar      p
type family XPInt      p
type family XPWildcard p

data PatternF p f
  = PCon (XPCon p) String [f]        -- ^ data constructor
  | PInt (XPInt p) Word              -- ^ integer literal
  | PVar (XPVar p) String            -- ^ variable
  | PImp (XPVar p) String            -- ^ {variable}
  | PImpTy (XPVar p) String (Expr p) -- ^ {variable : type}
  | PWildcard (XPWildcard p)         -- ^ wildcard "_"
  deriving Functor
