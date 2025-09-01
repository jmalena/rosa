module Language.Rosa.Parser.Errors where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Data.SourceSpan
import Language.Rosa.Error
import Language.Rosa.Parser.Token

data ParseError
  = UnexpectedChar SrcPos Char
  | UnexpectedToken Token
  | IntParserInternalError BL.ByteString
  deriving (Eq, Show)

instance CompilerError ParseError where
  compilerError = print
