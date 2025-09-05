{-# LANGUAGE TypeFamilies #-}

module Language.Rosa.Ast.Expr where

import Data.Word

import Language.Rosa.Ast.Pattern
import Language.Rosa.Core

data Expr
  = Var SrcSpan String           -- ^ variable reference
  | App SrcSpan Expr Expr        -- ^ function application
  | Pi SrcSpan Pattern Expr Expr -- ^ dependent function type (Π-type).
  | ValueBool SrcSpan Bool       -- ^ boolean literal
  | ValueInt SrcSpan Word64      -- ^ integer literal
  deriving (Eq, Show)

instance HasAnn Expr where
  type AnnType Expr = SrcSpan

  ann (Var a _)       = a
  ann (App a _ _)     = a
  ann (Pi a _ _ _)    = a
  ann (ValueBool a _) = a
  ann (ValueInt a _)  = a

  setAnn a' (Var _ x)        = Var a' x
  setAnn a' (App _ f args)   = App a' f args
  setAnn a' (Pi _ p ty body) = Pi a' p ty body
  setAnn a' (ValueBool _ x)  = ValueBool a' x
  setAnn a' (ValueInt _ x)   = ValueInt a' x
