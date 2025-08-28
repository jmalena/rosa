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
%error { parseError }

%token
  -- symbols
  '.'        { (TokSymbol ".", _) }

  -- keywords
  import     { (TokKeyword "import", _) }

  -- literals
  bool       { (TokBool _, _) }
  int        { (TokInt _, _) }

  -- identifier
  identifier { (TokIdent _, _) }
%%

Statement :: { Statement }
  : import ModulePath
    { Import (snd $1 <+> snd $2) (fst $2) }
          
ModulePath :: { (BL.ByteString, Span) }
  : identifier
    { (extractIdent (fst $1), snd $1) }
  | ModulePath '.' identifier
    { (fst $1 <> "." <> extractIdent (fst $3), snd $1 <+> snd $3) }
          
Literal :: { ValueLiteral }
  : bool
    { ValueBool (snd $1) (extractBool $ fst $1) }
  | int
    { ValueInt (snd $1) (extractInt $ fst $1) }               

{
parseError :: [Token] -> Parser a
parseError [] =
  throwRosaError UnexpectedEndOfInput
parseError (tok:_) =
  throwRosaError $ UnexpectedToken tok

parseSourceFile :: SourceFile -> Parser Statement
parseSourceFile srcFile = scanTokens srcFile >>= parseTokens
}
