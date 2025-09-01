module Language.Rosa.Parser.Token where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Word

import Language.Rosa.Data.ModulePath
import Language.Rosa.Data.SourceSpan

type Token = (SrcSpan, Tok)

data Tok
  = TokSymbol BL.ByteString
  | TokKeyword BL.ByteString
  
  -- literals
  | TokBool Bool
  | TokInt  Word64

  -- identifiers
  | TokIdent BL.ByteString
  | TokModulePath ModulePath

  -- EOF
  | TokEOF
 deriving (Eq, Show)
