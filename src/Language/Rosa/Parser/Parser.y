{
{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.Parser where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Ast
import Language.Rosa.Error
import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Lexer
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Token
import Language.Rosa.SourceFile
}

%name parseTokens Statement

%tokentype { Token }
%monad { Parser }
%errorhandlertype explist
%error { parseError }

%token
  -- symbols
  ';'        {(Semicolon, _)}
  '.'        {(Dot, _)}

  -- operators
  '='        {(Op Assign, _)}

  -- keywords
  import     {(KeywordImport, _)}
  let        {(KeywordLet, _)}

  -- literals
  bool       {(LiteralBool _, _)}
  int        {(LiteralInt _, _)}

  -- identifier
  identifier {(IdentifierKebabCase _, _)}
%%

Statement :: { Statement }
  : import ModulePath ';'
    { Import (snd $1 <+> snd $3) (fst $2) }
          
ModulePath :: { (BL.ByteString, Span) }
  : identifier
    { (extractKebabCase (fst $1), snd $1) }
  | ModulePath '.' identifier
    { (fst $1 <> "." <> extractKebabCase (fst $3), snd $1 <+> snd $3) }
          
Literal :: { ValueLiteral }
  : bool
    { ValueBool (snd $1) (extractBool $ fst $1) }
  | int
    { ValueInt (snd $1) (extractInt $ fst $1) }               

{
parseError :: ([Token], [String]) -> Parser a
parseError ([], _) =
  throwRosaError UnexpectedEndOfInput
parseError (tok:_, _) =
  throwRosaError $ UnexpectedToken (snd tok) (fst tok)

parseSourceFile :: SourceFile -> Parser Statement
parseSourceFile src = scanTokens src >>= parseTokens
}
