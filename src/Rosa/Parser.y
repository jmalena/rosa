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
  '='      { TokenAssign }
  if       { TokenIfKeyword }
  else     { TokenElseKeyword }
  for      { TokenFor }
  while    { TokenWhile }
  do       { TokenDo }
  break    { TokenBreak }
  continue { TokenContinue }
  return   { TokenRetKeyword }
  int      { TokenIntKeyword }
  LIT      { TokenLit $$ }
  IDENT    { TokenIdent $$ }
  ','      { TokenComma }

%left ','
%right '='
%left "||"
%left "&&"
%left "==" "!="
%left "<=" "<" ">=" ">"
%left '+' '-'
%left '*' '/'
%right UNARY_MINUS '!' '~' FUNC_CALL
%nonassoc THEN
%nonassoc else

%%

Program : {- -}                                                                  { [] }
        | FuncDecl                                                               { [$1] }
        | FuncDecl Program                                                       { $1:$2 }

FuncDecl : int IDENT '(' FuncDeclParams ')' ';'                                  { FuncDecl $2 $4 Nothing }
         | int IDENT '(' FuncDeclParams ')' '{' Block '}'                        { FuncDecl $2 $4 (Just $7) }

FuncDeclParams : {- -}                                                           { [] }
               | int IDENT                                                       { [$2] }
               | int IDENT ',' FuncDeclParams                                    { $2:$4 }

Block : {- empty -}                                                              { [] }
      | BlockItem Block                                                          { $1:$2 }

BlockItem : int IDENT ';'                                                        { BlockDecl $2 Nothing }
          | int IDENT '=' Expr ';'                                               { BlockDecl $2 (Just $4) }
          | Statement                                                            { BlockStmt $1 }

Statement : OptionalExpr ';'                                                     { SideEff $1 }
          | '{' Block '}'                                                        { Compound $2 }
          | if '(' Expr ')' Statement %prec THEN                                 { If $3 $5 Nothing }
          | if '(' Expr ')' Statement else Statement                             { If $3 $5 (Just $7) }
          | for '(' OptionalExpr ';' OptionalExpr ';' OptionalExpr ')' Statement { For $3 $5 $7 $9 }
          | while '(' Expr ')' Statement                                         { While $3 $5 }
          | do Statement while '(' Expr ')' ';'                                  { Do $2 $5 }
          | break ';'                                                            { Break }
          | continue ';'                                                         { Continue }
          | return Expr ';'                                                      { Return $2 }

OptionalExpr : {- empty -}                                                       { Nothing }
             | Expr                                                              { Just $1 }

FuncCallArgs : {- -}                                                             { [] }
             | Expr                                                              { [$1] }
             | Expr ',' FuncCallArgs                                             { $1:$3 }

Expr : IDENT '(' FuncCallArgs ')' %prec FUNC_CALL                                { FuncCall $1 $3 }
     | '-' Expr %prec UNARY_MINUS                                                { UnaryOp OpAddCompl $2 }
     | '!' Expr                                                                  { UnaryOp OpLogCompl $2 }
     | '~' Expr                                                                  { UnaryOp OpBitCompl $2 }
     | Expr '+' Expr                                                             { BinaryOp OpAdd $1 $3 }
     | Expr '-' Expr                                                             { BinaryOp OpSub $1 $3 }
     | Expr '*' Expr                                                             { BinaryOp OpMul $1 $3 }
     | Expr '/' Expr                                                             { BinaryOp OpDiv $1 $3 }
     | Expr "<=" Expr                                                            { BinaryOp OpLTE $1 $3 }
     | Expr "<" Expr                                                             { BinaryOp OpLT $1 $3 }
     | Expr ">=" Expr                                                            { BinaryOp OpGTE $1 $3 }
     | Expr ">" Expr                                                             { BinaryOp OpGT $1 $3 }
     | Expr "==" Expr                                                            { BinaryOp OpEQ $1 $3 }
     | Expr "!=" Expr                                                            { BinaryOp OpNEQ $1 $3 }
     | Expr "&&" Expr                                                            { BinaryOp OpLogAnd $1 $3 }
     | Expr "||" Expr                                                            { BinaryOp OpLogOr $1 $3 }
     | '(' Expr ')'                                                              { $2 }
     | IDENT '=' Expr                                                            { Assign $1 $3 }
     | IDENT                                                                     { Ref $1 }
     | LIT                                                                       { Lit64 $1 }

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

parse :: String -> Either String [Defn]
parse s = expr (tokenize s) "foo" 1
}
