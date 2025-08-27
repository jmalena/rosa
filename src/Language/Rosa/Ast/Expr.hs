module Language.Rosa.Ast.Expr where

import Data.Word

import Language.Rosa.Ast.Span

data Expr
  = TermBool Span Bool
  | TermInt Span Word64
