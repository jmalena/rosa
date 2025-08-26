module Language.Rosa.Ast.Span where

data Span = Span
  { startLine :: Int
  , startCol :: Int
  , endLine :: Int
  , endCol :: Int
  } deriving (Eq, Show)
