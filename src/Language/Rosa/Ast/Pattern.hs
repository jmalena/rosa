{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Pattern where

import Data.Word

import Language.Rosa.Core

data Pattern
  = PCon SrcSpan String [Pattern] -- ^ applied constructor
  | PVar SrcSpan String           -- ^ variable name
  | PInt SrcSpan Word64           -- ^ integer literal
  | PWildcard SrcSpan             -- ^ wildcard "_"
  deriving (Eq, Show)

instance HasAnn Pattern where
  type AnnType Pattern = SrcSpan

  ann (PCon a _ _)  = a
  ann (PVar a _)    = a
  ann (PInt a _)    = a
  ann (PWildcard a) = a

  setAnn a' (PCon _ name args) = PCon a' name args
  setAnn a' (PVar _ name)      = PVar a' name
  setAnn a' (PInt _ x)         = PInt a' x
  setAnn a' (PWildcard _)      = PWildcard a'
