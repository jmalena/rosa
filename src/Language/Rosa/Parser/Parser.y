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
  '('          { Ann (_, TSymbol "(") }
  ')'          { Ann (_, TSymbol ")") }
  '_'          { Ann (_, TSymbol "_") }
  ':'          { Ann (_, TSymbol ":") }
  '->'         { Ann (_, TSymbol "->") }
  ':='         { Ann (_, TSymbol ":=") }

  -- keywords
  "import"       { Ann (_, TKeyword "import") }

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
  : many1(p)       { NE.toList $1 }
  | {- empty -}    { [] }

many1(p) :: { NE.NonEmpty p }
  : many1_rev(p)   { NE.reverse $1 }

many1_rev(p) :: { NE.NonEmpty p }
  : many1_rev(p) p { NE.cons $2 $1 }
  | p              { NE.singleton $1 }

------------------------------------------------------------
-- Rules
------------------------------------------------------------

module :: { Module }
  : many(import_decl) many(decl)
    { Module
      { moduleImports = $1
      , moduleDecls = $2
      }
    }

import_decl :: { ImportDecl }
  : "import" modulepath
    { let TModulePath mp = val $2
      in ImportDecl
        { importDeclAnn  = ann $1 <+> ann $2
        , importDeclPath = mp
        }
    }

decl :: { Decl }
  : ident ':' expr
    { let TIdent name = val $1
      in TypeDecl
         { typeDeclAnn = ann $1 <+> ann $3
         , typeDeclLhs = PVar (ann $1) name
         , typeDeclRhs = $3
         }
    }
  | ident many(pattern) ':=' expr
    { let TIdent name = val $1
      in DefDecl
         { defDeclAnn = (patSpan $1 $2) <+> ann $4
         , defDeclLhs = PCon (patSpan $1 $2) name $2
         , defDeclRhs = $4
         }
     }

-- | Top-level expression rule with "->" handling (right-associative).
expr :: { Expr }
  : app_expr '->' expr
    -- NOTE: `a -> b` is treated as a non-dependent Π-type where the argument is unused.
    { Pi (ann $1 <+> ann $3) (PWildcard (ann $1)) $1 $3 }
  | app_expr
    { $1 }

-- | Expression rule with function application (left-associative).
app_expr :: { Expr }
  : app_expr term
    { App (ann $1 <+> ann $2) $1 $2 }
  | term
    { $1 }

term :: { Expr }
  : bool
    { let TBool x = val $1
      in ValueBool (ann $1) x
    }
  | int
    { let TInt x = val $1
      in ValueInt (ann $1) x
    }
  | ident
    { let TIdent f = val $1
      in Var (ann $1) f
    }
  | '(' expr ')'
    { setAnn (ann $1 <+> ann $3) $2 }

pattern :: { Pattern }
  : ident
    { let TIdent name = val $1
      in PVar (ann $1) name
    }
  | int
    { let TInt x = val $1
      in PInt (ann $1) x
    }
  | '_'
    { PWildcard (ann $1) }
  | '(' pattern_inner ')'
    { setAnn (ann $1 <+> ann $3) $2 }

pattern_inner :: { Pattern }
  : ident many1(pattern)
    { let TIdent name = val $1
      in PCon (ann $1 <+> ann (NE.last $2)) name (NE.toList $2)
    }
  | pattern
    { $1 }

{
-- NOTE: we have this function because let with multiple values is somehow not working in Happy syntax...
patSpan id [] = ann id
patSpan id ps = ann id <+> ann (last ps)

lexer :: (Located Token -> Parser a) -> Parser a
lexer = (nextToken >>=)

parseError :: Located Token -> Parser a
parseError tok = throwRosaError $ UnexpectedToken tok
}
