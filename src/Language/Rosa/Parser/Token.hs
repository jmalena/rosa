module Language.Rosa.Parser.Token where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Word

import Language.Rosa.Data.ModulePath
import Language.Rosa.Data.SourceSpan

type Token = (SrcSpan, TokenClass)

data TokenClass
  -- special
  = TSymbol BL.ByteString
  | TKeyword BL.ByteString
  
  -- literals
  | TBool Bool
  | TInt  Word64

  -- identifiers
  | TIdent BL.ByteString
  | TModulePath ModulePath

  -- EOF
  | TEof
 deriving (Eq, Show)
