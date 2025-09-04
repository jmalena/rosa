module Language.Rosa.Parser.Token where

import Data.Word

import Language.Rosa.Core

data Token
  -- special
  = TSymbol String
  | TKeyword String
  
  -- literals
  | TBool Bool
  | TInt  Word64

  -- identifiers
  | TIdent String
  | TModulePath ModulePath

  -- EOF
  | TEof
 deriving (Eq, Show)
