module Language.Rosa.Ast.Statement where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Ast.Span

data Statement
  = Import Span BL.ByteString
