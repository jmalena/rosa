module Language.Rosa.Ast.Value where

import Data.Word

import Language.Rosa.Ast.Span

data ValueLiteral
  = ValueBool Span Bool
  | ValueInt Span Word64
  deriving (Eq, Show)
