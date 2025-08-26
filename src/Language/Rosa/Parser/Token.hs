module Language.Rosa.Parser.Token where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Ast

tokType :: Token -> TokenClass
tokType (t, _) = t

tokPos :: Token -> Span
tokPos (_, p) = p

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
  
  -- identifiers
  | KebabIdentifier BL.ByteString
 deriving (Eq, Show)
