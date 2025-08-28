module Language.Rosa.SourceFile where

import qualified Data.ByteString.Lazy.Char8 as BL

data SourceFile
  = FileSource FilePath BL.ByteString
  | StdinSource BL.ByteString

srcFileContent :: SourceFile -> BL.ByteString
srcFileContent (FileSource _ s) = s
srcFileContent (StdinSource s)  = s
