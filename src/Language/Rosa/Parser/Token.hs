module Language.Rosa.Parser.Token where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Word

import Language.Rosa.Ast

tokType :: Token -> TokenClass
tokType (t, _) = t

tokSpan :: Token -> Span
tokSpan (_, p) = p

type Token = (TokenClass, Span)

data TokenClass
  -- symbols
  = Semicolon
  
  -- operators
  | Op Operator

  -- keywords
  | KeywordLet
  
  -- literals
  | LiteralBool { extractBool :: Bool }
  | LiteralInt  { extractInt :: Word64 }

  -- identifiers
  | IdentifierKebabCase { extractKebabCase :: BL.ByteString }
 deriving (Eq, Show)
