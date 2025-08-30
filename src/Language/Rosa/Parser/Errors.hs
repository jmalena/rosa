module Language.Rosa.Parser.Errors where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Error
import Language.Rosa.Parser.Token

data ParseError
  = UnexpectedCharacter Char -- TokenizeError
  | UnexpectedToken Token
  | UnexpectedEndOfInput
  | IntParserInternalError BL.ByteString
  deriving (Eq, Show)

instance CompilerError ParseError where
  compilerError = print
