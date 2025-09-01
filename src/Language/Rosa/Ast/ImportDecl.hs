module Language.Rosa.Ast.ImportDecl where

import Language.Rosa.Data.ModulePath
import Language.Rosa.Data.SourceSpan

data ImportDecl = ImportDecl
  { importDeclMeta :: SrcSpan
  , importDeclPath  :: ModulePath
  }
  deriving (Eq, Show)
