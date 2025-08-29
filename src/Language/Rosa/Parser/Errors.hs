module Language.Rosa.Parser.Errors where

import Language.Rosa.Error
import Language.Rosa.Parser.Token

data LexerError
  = LexicalError String -- TODO: LexicalError value is "(line, col)"
  deriving (Eq, Show)

data ParseError
  = UnexpectedEndOfInput
  | UnexpectedToken Token
  deriving (Eq, Show)

instance CompilerError LexerError where
  compilerError = print

instance CompilerError ParseError where
  compilerError = print
