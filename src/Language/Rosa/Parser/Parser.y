{
module Language.Rosa.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.NonEmpty         as NE

import Language.Rosa.Ast
import Language.Rosa.Core
import Language.Rosa.Error
import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Lexer
import Language.Rosa.Parser.Token
}

%name parseModule module

%tokentype { Located Token }
%monad { Parser }
%lexer { lexer } { Ann (_, TEof) }
%error { parseError }

%token
  -- symbols
  ':='          { Ann (_, TSymbol ":=") }

  -- keywords
  import       { Ann (_, TKeyword "import") }

  -- literals
  bool         { Ann (_, TBool _) }
  int          { Ann (_, TInt _) }

  -- identifier
  ident        { Ann (_, TIdent _) }

  -- module path
  modulepath   { Ann (_, TModulePath _) }
%%

------------------------------------------------------------
-- Utils
------------------------------------------------------------

many(p) :: { [p] }
  : some(p)                             { NE.toList $1 }
  | {- empty -}                         { [] }

some(p) :: { NE.NonEmpty p }
  : some_rev(p)                         { NE.reverse $1 }

some_rev(p) :: { NE.NonEmpty p }
  : some_rev(p) p                       { NE.cons $2 $1 }
  | p                                   { NE.singleton $1 }

------------------------------------------------------------
-- Rules
------------------------------------------------------------

module :: { Module }
  : many(import_decl)
    { Module
      { moduleImports = $1
      , moduleDecls = []
      }
    }

import_decl :: { ImportDecl }
  : import modulepath
    { let TModulePath mp = val $2
        in ImportDecl
          { importDeclMeta = ann $1 <+> ann $2
          , importDeclPath = mp
          }
    }

expr :: { Expr }
  : bool
    { let TBool x = val $1
        in ValueBool (ann $1) x
    }
  | int
    { let TInt x = val $1
        in ValueInt (ann $1) x
    }

{
lexer :: (Located Token -> Parser a) -> Parser a
lexer = (nextToken >>=)

parseError :: Located Token -> Parser a
parseError tok = throwRosaError $ UnexpectedToken tok
}
