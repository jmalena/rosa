module Language.Rosa.Ast.Module where

import Language.Rosa.Ast.Decl

data Module p
  = Module
    { moduleDecls :: [Decl p]
    }
