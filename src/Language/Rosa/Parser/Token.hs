module Language.Rosa.Parser.Token where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Word

import Language.Rosa.Ast

type Token = (TokenClass, Span)

data TokenClass
  -- symbols
  = Semicolon
  | Dot
  
  -- operators
  | Op Operator

  -- keywords
  | KeywordImport
  | KeywordLet
  
  -- literals
  | LiteralBool { extractBool :: Bool }
  | LiteralInt  { extractInt :: Word64 }

  -- identifiers
  | IdentifierKebabCase { extractKebabCase :: BL.ByteString }
 deriving (Eq, Show)
