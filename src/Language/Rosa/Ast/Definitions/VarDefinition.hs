module Language.Rosa.Ast.Definitions.VarDefinition where

import qualified Data.ByteString.Lazy.Char8 as BL

data VarDefinition = VarDefinition
  { varName :: BL.ByteString
  , varBody :: ()
  }
