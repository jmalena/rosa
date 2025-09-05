module Language.Rosa.Ast.Module where

import Language.Rosa.Ast.Decl

data Module = Module
  { moduleDecls :: [Decl]
  }
  deriving (Eq, Show)
