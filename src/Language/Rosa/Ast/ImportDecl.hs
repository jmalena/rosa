module Language.Rosa.Ast.ImportDecl where

import Language.Rosa.Core

data ImportDecl = ImportDecl
  { importDeclMeta :: SrcSpan
  , importDeclPath  :: ModulePath
  }
  deriving (Eq, Show)
