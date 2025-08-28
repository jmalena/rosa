module Language.Rosa.Parser.Errors where

import Language.Rosa.Ast (Span)
import Language.Rosa.Error
import Language.Rosa.Parser.Token

data LexerError
  = LexicalError Int Int
  deriving (Eq, Show)

data ParseError
  = UnexpectedEndOfInput
  | UnexpectedToken Span TokenClass
  deriving (Eq, Show)

instance CompilerError LexerError where
  compilerError = print

instance CompilerError ParseError where
  compilerError = print
