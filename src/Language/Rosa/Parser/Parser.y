{
module Language.Rosa.Parser.Parser (
  componentP
) where

import Language.Rosa.Syntax.AST
import Language.Rosa.Syntax.Operators
import Language.Rosa.Syntax.Stages
import Language.Rosa.Parser.Lexer
}

%name component component

%tokentype { Token }

%monad { P }
%errorhandlertype explist
%error { parseError }

%token
  '('      { TokLPar }
  ')'      { TokRPar }
  '{'      { TokLBra }
  '}'      { TokRBra }
  ';'      { TokSemi }
  '!'      { TokNeg }
  '*'      { TokMul }
  '/'      { TokDiv }
  '+'      { TokPlus }
  '-'      { TokMinus }
  "<"      { TokLt }
  ">"      { TokGt }
  "<="     { TokLte }
  ">="     { TokGte }
  "=="     { TokEq }
  "!="     { TokNeq }
  "&&"     { TokAnd }
  "||"     { TokOr }
  '='      { TokAssign }
  if       { TokIf }
  else     { TokElse }
  for      { TokFor }
  while    { TokWhile }
  do       { TokDo }
  break    { TokBreak }
  continue { TokContinue }
  return   { TokRet }
  int      { TokInt }
  LIT      { TokLit $$ }
  IDENT    { TokIdent $$ }
  ','      { TokComma }

%left ','
%right '='
%left "||"
%left "&&"
%left "==" "!="
%left "<=" "<" ">=" ">"
%left '+' '-'
%left '*' '/'
%right UNARY_PLUS UNARY_MINUS '!' FUNC_CALL
%nonassoc THEN
%nonassoc else

%%
component
    : {- empty -}                                                          { [] }
    | decl                                                                 { [$1] }
    | decl component                                                       { $1:$2 }

decl
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
    | '+' Expr %prec UNARY_PLUS                                            { UnaryOp () OpPlus $2 }
    | '-' Expr %prec UNARY_MINUS                                           { UnaryOp () OpMin $2 }
    | '!' Expr                                                             { UnaryOp () OpNeg $2 }
    | Expr '+' Expr                                                        { BinaryOp () OpAdd $1 $3 }
    | Expr '-' Expr                                                        { BinaryOp () OpSub $1 $3 }
    | Expr '*' Expr                                                        { BinaryOp () OpMul $1 $3 }
    | Expr '/' Expr                                                        { BinaryOp () OpDiv $1 $3 }
    | Expr "<" Expr                                                        { BinaryOp () OpLt $1 $3 }
    | Expr ">" Expr                                                        { BinaryOp () OpGt $1 $3 }
    | Expr "<=" Expr                                                       { BinaryOp () OpLte $1 $3 }
    | Expr ">=" Expr                                                       { BinaryOp () OpGte $1 $3 }
    | Expr "==" Expr                                                       { BinaryOp () OpEq $1 $3 }
    | Expr "!=" Expr                                                       { BinaryOp () OpNeq $1 $3 }
    | Expr "&&" Expr                                                       { BinaryOp () OpAnd $1 $3 }
    | Expr "||" Expr                                                       { BinaryOp () OpOr $1 $3 }
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

componentP :: [Token] -> P (Component Parsed)
componentP = component
}
