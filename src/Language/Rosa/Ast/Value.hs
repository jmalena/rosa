module Language.Rosa.Ast.Value where

import Data.Word

import Language.Rosa.Data.SourceSpan

data ValueLiteral
  = ValueBool SrcSpan Bool
  | ValueInt SrcSpan Word64
  deriving (Eq, Show)
