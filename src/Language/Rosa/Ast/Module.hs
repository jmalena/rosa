module Language.Rosa.Ast.Module where

import Language.Rosa.Ast.Decl
import Language.Rosa.Ast.ImportDecl

data Module = Module
  { moduleImports :: [ImportDecl]
  , moduleDecls :: [Decl]
  }
  deriving (Eq, Show)
