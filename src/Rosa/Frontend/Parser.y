{
module Rosa.Frontend.Parser (
  parse
) where

import Data.Maybe

import Rosa.Frontend.AST
import Rosa.Frontend.Lexer
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
    | FuncDecl                                                             { [$1] }
    | FuncDecl Program                                                     { $1:$2 }

FuncDecl
    : int Ident '(' FuncDeclParams ')' ';'                                 { FuncDecl $2 $4 Nothing }
    | int Ident '(' FuncDeclParams ')' '{' Block '}'                       { FuncDecl $2 $4 (Just $7) }

FuncDeclParams
    : {- empty -}                                                          { [] }
    | int Ident                                                            { [$2] }
    | int Ident ',' FuncDeclParams                                         { $2:$4 }

Block
    : {- empty -}                                                          { [] }
    | BlockItem Block                                                      { $1:$2 }

BlockItem
    : int Ident ';'                                                        { BlockDecl $2 Nothing }
    | int Ident '=' Expr ';'                                               { BlockDecl $2 (Just $4) }
    | Statement                                                            { BlockStmt $1 }

Statement
    : OptionalExpr ';'                                                     { ExprStmt $1 }
    | '{' Block '}'                                                        { Compound $2 }
    | if '(' Expr ')' Statement %prec THEN                                 { If $3 $5 Nothing }
    | if '(' Expr ')' Statement else Statement                             { If $3 $5 (Just $7) }
    | for '(' OptionalExpr ';' OptionalExpr ';' OptionalExpr ')' Statement { For $3 $5 $7 $9 }
    | while '(' Expr ')' Statement                                         { While $3 $5 }
    | do Statement while '(' Expr ')' ';'                                  { Do $2 $5 }
    | break ';'                                                            { Break }
    | continue ';'                                                         { Continue }
    | return Expr ';'                                                      { Return $2 }

OptionalExpr
    : {- empty -}                                                          { Nothing }
    | Expr                                                                 { Just $1 }

FuncCallArgs
    : {- empty -}                                                          { [] }
    | Expr                                                                 { [$1] }
    | Expr ',' FuncCallArgs                                                { $1:$3 }

Expr
    : Ident '(' FuncCallArgs ')' %prec FUNC_CALL                           { FuncCall $1 $3 }
    | '-' Expr %prec UNARY_MINUS                                           { UnaryOp OpAddCompl $2 }
    | '!' Expr                                                             { UnaryOp OpLogCompl $2 }
    | '~' Expr                                                             { UnaryOp OpBitCompl $2 }
    | Expr '+' Expr                                                        { BinaryOp OpAdd $1 $3 }
    | Expr '-' Expr                                                        { BinaryOp OpSub $1 $3 }
    | Expr '*' Expr                                                        { BinaryOp OpMul $1 $3 }
    | Expr '/' Expr                                                        { BinaryOp OpDiv $1 $3 }
    | Expr "<=" Expr                                                       { BinaryOp OpLTE $1 $3 }
    | Expr "<" Expr                                                        { BinaryOp OpLT $1 $3 }
    | Expr ">=" Expr                                                       { BinaryOp OpGTE $1 $3 }
    | Expr ">" Expr                                                        { BinaryOp OpGT $1 $3 }
    | Expr "==" Expr                                                       { BinaryOp OpEQ $1 $3 }
    | Expr "!=" Expr                                                       { BinaryOp OpNEQ $1 $3 }
    | Expr "&&" Expr                                                       { BinaryOp OpLogAnd $1 $3 }
    | Expr "||" Expr                                                       { BinaryOp OpLogOr $1 $3 }
    | '(' Expr ')'                                                         { $2 }
    | Ident '=' Expr                                                       { Assign $1 $3 }
    | Ident                                                                { Ref $1 }
    | LIT                                                                  { NumLit $1 }

Ident
    : IDENT                                                                { fromJust (mkIdent $1) }

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

parse :: String -> Either String [Defn]
parse s = runP (parseProgram (tokenize s)) "rosa" 1
}
