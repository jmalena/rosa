module Language.Rosa.Ast.Decl where

import Language.Rosa.Core
import Language.Rosa.Ast.Expr
import Language.Rosa.Ast.Pattern

data Decl
  = TypeDecl
    { typeDeclAnn :: SrcSpan
    , typeDeclLhs :: Pattern
    , typeDeclRhs :: Expr
    }
  | DefDecl
    { defDeclAnn :: SrcSpan
    , defDeclLhs :: Pattern
    , defDeclRhs :: Expr
    }
  deriving (Eq, Show)
