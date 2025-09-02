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

%tokentype { Token }
%monad { Parser }
%lexer { lexer } { (_, TEof) }
%error { parseError }

%token
  -- symbols
  '('          { (_, TSymbol "(") }
  ')'          { (_, TSymbol ")") }

  -- keywords
  import       { (_, TKeyword "import") }

  -- literals
  bool         { (_, TBool _) }
  int          { (_, TInt _) }

  -- identifier
  ident        { (_, TIdent _) }

  -- module path
  modulepath   { (_, TModulePath _) }
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
    { let TModulePath mp = snd $2
        in ImportDecl
          { importDeclMeta = fst $1 <> fst $2
          , importDeclPath = mp
          }
    }

{-
Literal :: { ValueLiteral }
  : bool
    { let TBool val = snd $1 in
        ValueBool (fst $1) val
    }
  | int
    { let TInt val = snd $1 in
        ValueInt (fst $1) val
    }
-}

{
lexer :: (Token -> Parser a) -> Parser a
lexer = (nextToken >>=)

parseError :: Token -> Parser a
parseError tok = throwRosaError $ UnexpectedToken tok
}
