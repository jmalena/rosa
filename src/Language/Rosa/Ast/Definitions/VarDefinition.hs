module Language.Rosa.Ast.Definitions.VarDefinition where

data VarDefinition = VarDefinition
  { varName :: String
  , varBody :: ()
  }
