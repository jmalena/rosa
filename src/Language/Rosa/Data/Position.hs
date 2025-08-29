module Language.Rosa.Data.Position where

-- | A position in a source file.
data Position
  = NoPos
  | Pos !Int !Int !Int -- ^ absolute offset, row, column
  deriving (Eq, Show)

-- | A region in a source file.
-- data Span
--  = Span !Position !Position -- ^ start position, end position
--  deriving (Eq, Show)
