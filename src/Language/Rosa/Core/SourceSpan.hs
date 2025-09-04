module Language.Rosa.Core.SourceSpan where

import qualified Data.ByteString.Lazy.Char8 as BL

type Located a = (SrcSpan, a)

-- | Represents a single position in a source file.
--   Line and column numbers are 1-based.
data SrcPos = SrcPos
  { line   :: !Int
  , column :: !Int
  } deriving (Eq, Ord, Show)

-- | Represents a span between two positions in a source file.
--   Invariant: start <= end.
data SrcSpan = SrcSpan
  { start :: !SrcPos
  , end   :: !SrcPos
  } deriving (Eq, Ord, Show)

instance Semigroup SrcSpan where
  (<>) (SrcSpan s1 e1) (SrcSpan s2 e2) =
    mkSpan (min s1 s2) (max e1 e2)

-- | Construct a source position (line and column must be positive).
mkPos :: Int -> Int -> SrcPos
mkPos l c
  | l < 1 || c < 1 = error "Position must have positive line and column"
  | otherwise      = SrcPos l c

-- | Construct a source span from two positions.
mkSpan :: SrcPos -> SrcPos -> SrcSpan
mkSpan p1 p2
  | p1 <= p2  = SrcSpan p1 p2
  | otherwise = SrcSpan p2 p1

-- | Advance a source position by one character.
advancePos :: SrcPos -> Char -> SrcPos
advancePos (SrcPos l c) ch
  | ch == '\n' = SrcPos (l + 1) 1
  | ch == '\r' = SrcPos l c
  | otherwise  = SrcPos l (c + 1)

-- | Advance a source position over an string.
advanceOver :: SrcPos -> String -> SrcPos
advanceOver = foldl advancePos

-- | Construct a source span of a string on given position.
spanOver :: SrcPos -> String -> SrcSpan
spanOver p s = mkSpan p (p `advanceOver` s)

