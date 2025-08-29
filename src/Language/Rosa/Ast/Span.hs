{-# LANGUAGE PatternSynonyms #-}

module Language.Rosa.Ast.Span where

data Span = Span
  { startLine :: Int
  , startCol  :: Int
  , endLine   :: Int
  , endCol    :: Int
  } deriving (Eq, Show)

pattern NoSpan :: Span
pattern NoSpan = Span
  { startLine = 0
  , startCol  = 0
  , endLine   = 0
  , endCol    = 0
  }

instance Semigroup Span where
  (Span sl1 sc1 el1 ec1) <> (Span sl2 sc2 el2 ec2) = Span
    { startLine = min sl1 sl2
    , startCol  = min sc1 sc2
    , endLine   = max el1 el2
    , endCol    = max ec1 ec2
    }
