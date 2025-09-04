{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Expr where

import Data.Word

import Language.Rosa.Core

data Expr
  = ValueBool SrcSpan Bool
  | ValueInt SrcSpan Word64
  deriving (Eq, Show)

instance HasAnn Expr where
  type AnnType Expr = SrcSpan

  ann (ValueBool sp _) = sp
  ann (ValueInt sp _) = sp

  setAnn sp' (ValueBool _ x) = ValueBool sp' x
  setAnn sp' (ValueInt _ x) = ValueInt sp' x
