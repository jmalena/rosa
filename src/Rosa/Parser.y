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
  '~'      { TokenBitCompl }
  '!'      { TokenLogCompl }
  '+'      { TokenPlus }
  '-'      { TokenMinus }
  '*'      { TokenMul }
  '/'      { TokenDiv }
  return   { TokenRetKeyword }
  int      { TokenIntKeyword }
  INT      { TokenInt $$ }
  IDENT    { TokenIdent $$ }


%%
Program : FunctionDecl                               { $1 }

FunctionDecl : int IDENT '(' ')' '{' Statement '}'   { Func $2 [$6] }

Statement : return Expr ';'                          { Return $2 }

Expr : Expr '+' Term                                 { BinaryOp OpAdd $1 $3 }
     | Expr '-' Term                                 { BinaryOp OpSub $1 $3 }
     | Term                                          { $1 }


Term : Term '*' Factor                               { BinaryOp OpMul $1 $3 }
     | Term '/' Factor                               { BinaryOp OpDiv $1 $3 }
     | Factor                                        { $1 }

Factor : '(' Expr ')'                                { $2 }
       | UnaryOp Factor                              { UnaryOp $1 $2 }
       | INT                                         { Lit (LInt64 $1) }

UnaryOp : '~'                                        { OpBitCompl }
        | '!'                                        { OpLogCompl }
        | '-'                                        { OpAddCompl }

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
