module Language.Rosa.Ast.Statement where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Data.SourceSpan

data Statement
  = Import SrcSpan BL.ByteString
  deriving (Eq, Show)
