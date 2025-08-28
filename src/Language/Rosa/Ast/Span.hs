{-# LANGUAGE PatternSynonyms #-}

module Language.Rosa.Ast.Span where

data Span = Span
  { startLine :: Int
  , startCol :: Int
  , endLine :: Int
  , endCol :: Int
  } deriving (Eq, Show)

pattern NoPos :: Span
pattern NoPos = Span { startLine = 0, startCol = 0, endLine = 0, endCol = 0 }

(<+>) :: Span -> Span -> Span
(<+>) span1 NoPos = span1
(<+>) NoPos span2 = span2
(<+>) span1 span2 = Span
  { startLine     = fst min
  , startCol      = snd min
  , endLine       = fst max
  , endCol        = snd max
  }
 where
  a1  = (startLine span1, startCol span1)
  a2  = (endLine span1, endCol span1)
  b1  = (startLine span2, startCol span2)
  b2  = (endLine span2, endCol span2)
  min = if a1 < b1 then a1 else b1
  max = if a2 > b2 then a2 else b2
