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
  "use"        { Ann (_, TKeyword "use") }

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

------------------------------------------------------------
-- Module
------------------------------------------------------------

module :: { Module }
  : many_sep(use_decl, newlines) optional(newlines) many_sep(decl, newlines)
    { Module
      { moduleDecls = $1 ++ $3
      }
    }

------------------------------------------------------------
-- Top-level declarations
------------------------------------------------------------

-- | Top-level declaration for module import.
use_decl :: { Decl }
  : "use" module_path
    { UseModule
      { useModuleAnn  = ann $1 <+> ann $2
      , useModulePath = val $2
      }
    }

-- | Top-level declaration (except for the "use module" rule).
decl :: { Decl }
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

-- | Expression rule with "->" handling (right-associative).
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
    { let TBool b = val $1
      in ValueBool (ann $1) b
    }
  | int
    { let TInt i = val $1
      in ValueInt (ann $1) i
    }
  | ident
    { let TIdent s = val $1
      in Var (ann $1) s
    }
  | '(' expr ')'
    { setAnn (ann $1 <+> ann $3) $2 }

------------------------------------------------------------
-- Patterns
------------------------------------------------------------

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

------------------------------------------------------------
-- Others
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

{
lexer :: (Located Token -> Parser a) -> Parser a
lexer = (nextToken >>=)

parseError :: Located Token -> Parser a
parseError tok = throwRosaError $ UnexpectedToken tok
}
