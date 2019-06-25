module Rosa.Codegen.Frame (
  Frame(size),
  empty,
  alloc64,
  dealloc64,
  markVar,
  findVarOffset
) where

import Data.Int
import qualified Data.Map as Map

data Frame = Frame
  { size :: Int64
  , variables :: Map.Map String Int64
  } deriving (Eq, Show)

empty :: Frame
empty = Frame
  { size = 0
  , variables = Map.empty }

alloc64 :: Frame -> (Int64, Frame)
alloc64 frame =
  ( size frame
  , frame { size = (size frame) + 8 }
  )

dealloc64 :: Frame -> Frame
dealloc64 frame =
  frame { size = (size frame) - 8}

markVar :: String -> Int64 -> Frame -> Frame
markVar ident offset frame =
  frame { variables = Map.insert ident offset (variables frame) }

findVarOffset :: String -> Frame -> Maybe Int64
findVarOffset ident frame =
  (variables frame) Map.!? ident
