{
module Language.Rosa.Frontend.Parser (
  parse
) where

import Data.Maybe

import Language.Rosa.Frontend.AST
import Language.Rosa.Frontend.Lexer
}

%name parseProgram Program
%tokentype { Token }

%monad { P }
%errorhandlertype explist
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
Program
    : {- empty -}                                                          { [] }
    | ExternDecl                                                           { [$1] }
    | ExternDecl Program                                                   { $1:$2 }

ExternDecl
    : int Ident '(' FuncParamDecls ')' ';'                                 { FuncDecl () $2 $4 }
    | int Ident '(' FuncParamDecls ')' BlockStmt                           { FuncDefn () $2 $4 $6 }

BlockStmt
    : '{' ScopedStmts '}'                                                  { $2 }
    
ScopedStmts
    : {- empty -}                                                          { [] }
    | ScopedStmt ScopedStmts                                               { $1:$2 }

ScopedStmt
    : int Ident ';'                                                        { VarDecl () $2 }
    | int Ident '=' Expr ';'                                               { VarDefn () $2 $4 }
    | Stmt                                                                 { Stmt () $1 }

Stmt
    : if '(' Expr ')' Stmt %prec THEN                                      { If () $3 $5 Nothing }
    | if '(' Expr ')' Stmt else Stmt                                       { If () $3 $5 (Just $7) }
    | for '(' OptionalExpr ';' OptionalExpr ';' OptionalExpr ')' Stmt      { For () $3 $5 $7 $9 }
    | while '(' Expr ')' Stmt                                              { While () $3 $5 }
    | do Stmt while '(' Expr ')' ';'                                       { Do () $2 $5 }
    | break ';'                                                            { Break () }
    | continue ';'                                                         { Continue () }
    | return Expr ';'                                                      { Return () $2 }
    | BlockStmt                                                            { Block () $1 }
    | Expr ';'                                                             { Expr () $1 }
    | ';'                                                                  { Noop () }

OptionalExpr
    : {- empty -}                                                          { Nothing }
    | Expr                                                                 { Just $1 }

FuncParamDecls
    : {- empty -}                                                          { [] }
    | int Ident                                                            { [$2] }
    | int Ident ',' FuncParamDecls                                         { $2:$4 }
    
FuncCallArgs
    : {- empty -}                                                          { [] }
    | Expr                                                                 { [$1] }
    | Expr ',' FuncCallArgs                                                { $1:$3 }

Expr
    : Ident '(' FuncCallArgs ')' %prec FUNC_CALL                           { FuncCall () $1 $3 }
    | '-' Expr %prec UNARY_MINUS                                           { UnaryOp () OpAddCompl $2 }
    | '!' Expr                                                             { UnaryOp () OpLogCompl $2 }
    | '~' Expr                                                             { UnaryOp () OpBitCompl $2 }
    | Expr '+' Expr                                                        { BinaryOp () OpAdd $1 $3 }
    | Expr '-' Expr                                                        { BinaryOp () OpSub $1 $3 }
    | Expr '*' Expr                                                        { BinaryOp () OpMul $1 $3 }
    | Expr '/' Expr                                                        { BinaryOp () OpDiv $1 $3 }
    | Expr "<=" Expr                                                       { BinaryOp () OpLTE $1 $3 }
    | Expr "<" Expr                                                        { BinaryOp () OpLT $1 $3 }
    | Expr ">=" Expr                                                       { BinaryOp () OpGTE $1 $3 }
    | Expr ">" Expr                                                        { BinaryOp () OpGT $1 $3 }
    | Expr "==" Expr                                                       { BinaryOp () OpEQ $1 $3 }
    | Expr "!=" Expr                                                       { BinaryOp () OpNEQ $1 $3 }
    | Expr "&&" Expr                                                       { BinaryOp () OpLogAnd $1 $3 }
    | Expr "||" Expr                                                       { BinaryOp () OpLogOr $1 $3 }
    | '(' Expr ')'                                                         { $2 }
    | Ident '=' Expr                                                       { Assign () $1 $3 }
    | Ident                                                                { Ref () $1 }
    | LIT                                                                  { NumLit () $1 }

Ident
    : IDENT                                                                { $1 }

{
newtype P a = P { runP :: String -> Int -> Either String a }

instance Functor P where
  fmap f (P m) = P $ \s l -> fmap f (m s l)

instance Applicative P where
  pure x = P $ \_ _ -> Right x
  (P f) <*> (P x) = P $ \s l ->
    case f s l of
      Left e -> Left e
      Right g -> fmap g (x s l)

instance Monad P where
  (P m) >>= k = P $ \s l ->
    case m s l of
      Left e -> Left e
      Right v -> runP (k v) s l

parseError :: ([Token], [String]) -> P a
parseError (toks, _) = P $ \_ l ->
  Left ("Parse error on line " ++ show l ++ " near " ++ show toks)

parse :: String -> Either String [ExternDecl Parsed]
parse s = runP (parseProgram (tokenize s)) "rosa" 1
}
