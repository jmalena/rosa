{
module Rosa.Parser (
  parse
) where

import Rosa.AST
import Rosa.Lexer
}

%name expr
%tokentype { Token }
%monad { P } { thenP } { returnP }
%error { parseError }

%token
  '('      { TokenLParen }
  ')'      { TokenRParen }
  '{'      { TokenLBracket }
  '}'      { TokenRBracket }
  ';'      { TokenSemi }
  return   { TokenRetKeyword }
  int      { TokenIntKeyword }
  INT      { TokenInt $$ }
  IDENT    { TokenIdent $$ }


%%
Program : Function                               { $1 }

Function : int IDENT '(' ')' '{' Statement '}'   { Func $2 [$6] }

Statement : return Expr ';'                      { Return $2 }

Expr : INT                                       { Lit (LInt $1) }

{
type P a = String -> Int -> Either String a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l ->
  case m s l of
    Left s -> Left s
    Right a -> k a s l

returnP :: a -> P a
returnP a = \s l -> Right a

parseError :: [Token] -> P a
parseError tok = \s l -> error ("Parse error on line " ++ show l ++ show tok ++ "\n")

parse :: String -> Either String Defn
parse s = expr (tokenize s) "foo" 1
}
