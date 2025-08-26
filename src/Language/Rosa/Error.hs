module Language.Rosa.Error where

import Language.Rosa.Ast

data RosaError
  = ParseError String (Maybe Span)
  deriving (Eq, Show)
