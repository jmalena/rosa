{
module Language.Rosa.Parser.Parser where

import Language.Rosa.Ast
import Language.Rosa.Error
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Token
}

%name moduleP Statement

%tokentype { Token }
%monad { Parser }
%errorhandlertype explist
%error { parseError }

%token
  -- symbols
  ';'        {(Semicolon, _)}

  -- operators
  '='        {(Op Assign, _)}

  -- keywords 
  "let"      {(KeywordLet, _)}

  -- literals
  bool       {(LiteralBool _, _)}
  int        {(LiteralInt _, _)}

  -- identifier
  identifier {(IdentifierKebabCase _, _)}
%%

Statement :: { Expr }
  : Expr { $1 }                

Expr :: { Expr }
  : bool { TermBool (tokSpan $1) (extractBool $ tokType $1) }
  | int  { TermInt (tokSpan $1) (extractInt $ tokType $1) }

{
-- TODO: rewrite the ParserError prettyPrint          
parseError :: ([Token], [String]) -> Parser a
parseError (a, b) = ParserFail $ ParseError ("Unexpected end of input") Nothing
parseError (tok:_, _) = ParserFail $ ParseError ("Unexpected " ++ (show $ tokType tok)) (Just $ tokSpan tok)
}
