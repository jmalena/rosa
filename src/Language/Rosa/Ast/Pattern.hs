{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Pattern where

type family XPCon      p
type family XPVar      p
type family XPInt      p
type family XPWildcard p

data Pattern p
  = PCon (XPCon p) String [Pattern p] -- ^ applied constructor
  | PVar (XPVar p) String             -- ^ variable name
  | PInt (XPInt p) Word               -- ^ integer literal
  | PWildcard (XPWildcard p)          -- ^ wildcard "_"
