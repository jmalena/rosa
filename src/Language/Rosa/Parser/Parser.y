{
module Language.Rosa.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List.NonEmpty         as NE

import Language.Rosa.Ast
import Language.Rosa.Core
import Language.Rosa.Error
import Language.Rosa.Parser.Annotations
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
  '{'          { Ann (_, TSymbol "{") }
  '}'          { Ann (_, TSymbol "}") }
  '_'          { Ann (_, TSymbol "_") }
  ':'          { Ann (_, TSymbol ":") }
  '->'         { Ann (_, TSymbol "->") }
  ':='         { Ann (_, TSymbol ":=") }
  '.'          { Ann (_, TSymbol ".") }
  '\\'         { Ann (_, TSymbol "\\") }

  -- keywords
  "use"        { Ann (_, TKeyword "use") }
  "let"        { Ann (_, TKeyword "let") }
  "in"         { Ann (_, TKeyword "in") }
  "type"       { Ann (_, TKeyword "type") }

  -- literals
  bool         { Ann (_, TBool _) }
  int          { Ann (_, TInt _) }

  -- identifiers
  ident        { Ann (_, TIdent _) }

  -- module paths
  modulepath   { Ann (_, TModulePath _) }

  -- structural
  newlines     { Ann (_, TNewlines ) }
%%

------------------------------------------------------------
-- Module
------------------------------------------------------------

module :: { Module ParserPhase }
  : many_sep(use_decl, newlines) optional(newlines) many_sep(decl, newlines)
    { Module
      { moduleDecls = $1 ++ $3
      }
    }

------------------------------------------------------------
-- Declarations (top-level)
------------------------------------------------------------

use_decl :: { Decl ParserPhase }
  : "use" module_path
    { UseModule
      { useModuleAnn  = ann $1 <+> ann $2
      , useModulePath = val $2
      }
    }

decl :: { Decl ParserPhase }
  -- type signature
  : ident ':' expr
    { let TIdent id = val $1
      in TySign
         { tySignAnn = ann $1 <+> ann $3
         , tySignId  = id
         , tySignRhs = $3
         }
    }
  -- pattern bind
  | pattern ':=' expr
    { PatBind
      { patBindAnn = ann $1 <+> ann $3
      , patBindLhs = $1
      , patBindRhs = $3
      }
    }
  -- function bind
  | ident many1(pattern) ':=' expr
    { let TIdent id = val $1
      in FunBind
         { funBindAnn     = ann $1 <+> ann $4
         , funBindId      = id
         , funBindMatches = $2
         , funBindRhs     = $4
         }
    }

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

expr :: { Expr ParserPhase }
  : app_expr ':' expr
    { Fix (Asc (ann $1 <+> ann $3) $1 $3) }
  | app_expr '->' expr
    { Fix (Pi (ann $1 <+> ann $3) (Fix (PWildcard (ann $1))) $1 $3) }
  | app_expr
    { $1 }

app_expr :: { Expr ParserPhase }
  : app_expr term
    { Fix (App (ann $1 <+> ann $2) $1 $2) }
  | term
    { $1 }

term :: { Expr ParserPhase }
  : "type"
    { Fix (Uni (ann $1) 0) }
  | "type" int
    { let TInt n = val $2
      in Fix (Uni (ann $1 <+> ann $2) (fromIntegral n))
    }
  | bool
    { let TBool b = val $1
      in Fix (BoolLit (ann $1) b)
    }
  | int
    { let TInt n = val $1
      in Fix (IntLit (ann $1) n)
    }
  | ident
    { let TIdent s = val $1
      in Fix (Var (ann $1) s)
    }
  | '{' expr '}'
    { Fix (Imp (ann $1 <+> ann $3) $2) }
  | '\\' pattern '.' expr
    { Fix (Abs (ann $1 <+> ann $4) $2 $4) }
  | '(' pattern ':' expr ')' '->' expr
    { Fix (Pi (ann $1 <+> ann $6) $2 $4 $7) }
  | "let" pattern ':=' expr "in" expr
    { Fix (Let (ann $1 <+> ann $6) $2 $4 $6) }
  | '(' expr ')'
    { setAnn (ann $1 <+> ann $3) $2 }


------------------------------------------------------------
-- Patterns
------------------------------------------------------------

pattern :: { Pattern ParserPhase }
  : '_'
    { Fix (PWildcard (ann $1)) }
  | int
    { let TInt x = val $1
      in Fix (PInt (ann $1) x)
    }
  | ident
    { let TIdent name = val $1
      in Fix (PVar (ann $1) name)
    }
  | '{' ident '}'
    { let TIdent s = val $2
      in Fix (PVarImp (ann $1 <+> ann $3) s Nothing)
    }
  | '{' ident ':' expr '}'
    { let TIdent s = val $2
      in Fix (PVarImp (ann $1 <+> ann $5) s (Just $4))
    }
  | '(' pattern_inner ')'
    { setAnn (ann $1 <+> ann $3) $2 }

pattern_inner :: { Pattern ParserPhase }
  : ident many1(pattern)
    { let TIdent name = val $1
      in Fix (PCon (ann $1 <+> ann (NE.last $2)) name (NE.toList $2))
    }
  | pattern
    { $1 }

------------------------------------------------------------
-- Module paths
------------------------------------------------------------

module_path :: { Located ModulePath }
  : ident
    { let TIdent s = val $1
      in (ann $1) @: readModulePath s
    }
  | modulepath
    { let TModulePath mp = val $1
      in (ann $1) @: mp
    }

------------------------------------------------------------
-- Utils
------------------------------------------------------------

optional(p) :: { Maybe p }
  : p                           { Just $1 }
  | {- empty -}                 { Nothing }

many(p) :: { [p] }
  : many1(p)                    { NE.toList $1 }
  | {- empty -}                 { [] }

many1(p) :: { NE.NonEmpty p }
  : many1_rev(p)                { NE.reverse $1 }

many1_rev(p) :: { NE.NonEmpty p }
  : many1_rev(p) p              { NE.cons $2 $1 }
  | p                           { NE.singleton $1 }

many_sep(p, sep) :: { [p] }
  : many1_sep(p, sep)           { NE.toList $1 }
  | {- empty -}                 { [] }

many1_sep(p, sep) :: { NE.NonEmpty p }
  : many1_sep_rev(p, sep)       { NE.reverse $1 }

many1_sep_rev(p, sep) :: { NE.NonEmpty p }
  : many1_sep_rev(p, sep) sep p { NE.cons $3 $1 }
  | p                           { NE.singleton $1 }

{
lexer :: (Located Token -> Parser a) -> Parser a
lexer = (nextToken >>=)

parseError :: Located Token -> Parser a
parseError tok = throwRosaError $ UnexpectedToken tok
}
