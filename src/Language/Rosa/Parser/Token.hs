module Language.Rosa.Parser.Token where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Word

import Language.Rosa.Ast

type Token = (Span, Tok)

data Tok
  = TokSymbol BL.ByteString
  | TokKeyword BL.ByteString
  
  -- literals
  | TokBool { extractBool :: Bool }
  | TokInt  { extractInt :: Word64 }

  -- identifiers
  | TokIdent { extractIdent :: BL.ByteString }
  | TokModulePath { extractModulePath :: BL.ByteString }

  -- EOF
  | TokEOF
 deriving (Eq, Show)
