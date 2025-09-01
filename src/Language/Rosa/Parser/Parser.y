{
{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Ast
import Language.Rosa.Error
import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Lexer
import Language.Rosa.Parser.Token
import Language.Rosa.SourceFile
}

%name parseModule Statement

%tokentype { Token }
%monad { Parser }
%lexer { lexer } { (_, TokEOF) }
%error { parseError }

%token
  -- symbols
  '('          { (_, TokSymbol "(") }
  ')'          { (_, TokSymbol ")") }

  -- keywords
  import       { (_, TokKeyword "import") }

  -- literals
  bool         { (_, TokBool _) }
  int          { (_, TokInt _) }

  -- identifier
  ident        { (_, TokIdent _) }

  -- module path
  modulepath   { (_, TokModulePath _) }
%%

Statement :: { Statement }
  : import modulepath
    { Import (fst $1 <> fst $2) (extractModulePath $ snd $2) }

Literal :: { ValueLiteral }
  : bool
    { ValueBool (fst $1) (extractBool $ snd $1) }
  | int
    { ValueInt (fst $1) (extractInt $ snd $1) }

{
lexer :: (Token -> Parser a) -> Parser a
lexer = (nextToken >>=)

parseError :: Token -> Parser a
parseError tok = throwRosaError $ UnexpectedToken tok
}
