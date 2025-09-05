module Language.Rosa.Core.InputSource where

import qualified Data.ByteString.Lazy.Char8 as BL

-- | Represents a source of input for the compiler.
data InputSource
  = FileInput FilePath BL.ByteString
  | InlineInput String

sourceContent :: InputSource -> BL.ByteString
sourceContent (FileInput _ bs) = bs
sourceContent (InlineInput s)  = BL.pack s
