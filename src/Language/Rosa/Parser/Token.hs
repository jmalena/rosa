module Language.Rosa.Parser.Token where

import Language.Rosa.Core

data Token
  -- special
  = TSymbol String
  | TKeyword String
  
  -- literals
  | TBool Bool
  | TInt Word

  -- identifiers
  | TIdent String

  -- module paths
  | TModulePath ModulePath

  -- structural
  | TNewlines
  | TEof
 deriving (Eq, Show)
