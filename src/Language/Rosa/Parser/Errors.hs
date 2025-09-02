module Language.Rosa.Parser.Errors where

import Language.Rosa.Core
import Language.Rosa.Error
import Language.Rosa.Parser.Token

data ParseError
  = UnexpectedChar SrcPos Char
  | UnexpectedToken Token
  | IntParserInternalError String
  deriving (Eq, Show)

instance CompilerError ParseError where
  compilerError = print
