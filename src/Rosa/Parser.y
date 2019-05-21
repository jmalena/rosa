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
  '*'      { TokenMul }
  '/'      { TokenDiv }
  '+'      { TokenPlus }
  '-'      { TokenMinus }
  "<="     { TokenLTE }
  "<"      { TokenLT }
  ">="     { TokenGTE }
  ">"      { TokenGT }
  "=="     { TokenEQ }
  "!="     { TokenNEQ }
  "&&"     { TokenLogAnd }
  "||"     { TokenLogOr }
  return   { TokenRetKeyword }
  int      { TokenIntKeyword }
  INT      { TokenInt $$ }
  IDENT    { TokenIdent $$ }

%left UNARY
%left '*' '/'
%left '+' '-'
%left "<=" "<" ">=" ">"
%left "==" "!="
%left "&&"
%left "||"

%%

Program : FunctionDecl                               { $1 }

FunctionDecl : int IDENT '(' ')' '{' Statement '}'   { Func $2 [$6] }

Statement : return Expr ';'                          { Return $2 }

Expr : Expr '+' Expr                                 { BinaryOp OpAdd $1 $3 }
     | Expr '-' Expr                                 { BinaryOp OpSub $1 $3 }
     | Expr '*' Expr                                 { BinaryOp OpMul $1 $3 }
     | Expr '/' Expr                                 { BinaryOp OpDiv $1 $3 }
     | UnaryOp Expr %prec UNARY                      { UnaryOp $1 $2 }
     | Expr "<=" Expr                                { BinaryOp OpLTE $1 $3 }
     | Expr "<" Expr                                 { BinaryOp OpLT $1 $3 }
     | Expr ">=" Expr                                { BinaryOp OpGTE $1 $3 }
     | Expr ">" Expr                                 { BinaryOp OpGT $1 $3 }
     | Expr "==" Expr                                { BinaryOp OpEQ $1 $3 }
     | Expr "!=" Expr                                { BinaryOp OpNEQ $1 $3 }
     | Expr "&&" Expr                                { BinaryOp OpLogAnd $1 $3 }
     | Expr "||" Expr                                { BinaryOp OpLogOr $1 $3 }
     | '(' Expr ')'                                  { $2 }
     | INT                                           { LInt64 $1 }

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
