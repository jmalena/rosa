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
  return   { TokenRetKeyword }
  int      { TokenIntKeyword }
  LIT      { TokenLit $$ }
  IDENT    { TokenIdent $$ }

%right THEN else -- see https://stackoverflow.com/questions/12731922/reforming-the-grammar-to-remove-shift-reduce-conflict-in-if-then-else
%left UNARY
%left '*' '/'
%left '+' '-'
%left "<=" "<" ">=" ">"
%left "==" "!="
%left "&&"
%left "||"
%right '='

%%

Program : FunctionDecl                               { [$1] }

FunctionDecl : int IDENT '(' ')' '{' Block '}'       { Func $2 $6 }

Block : {- empty -}                                  { [] }
      | BlockItem Block                              { $1:$2 }

BlockItem : int IDENT ';'                            { BlockDecl $2 Nothing }
          | int IDENT '=' Expr ';'                   { BlockDecl $2 (Just $4) }
          | Statement                                { BlockStmt $1 }

Statement : Expr ';'                                 { SideEff $1 }
          | if '(' Expr ')' Statement %prec THEN     { If $3 $5 Nothing }
          | if '(' Expr ')' Statement else Statement { If $3 $5 (Just $7) }
          | '{' Block '}'                            { Compound $2 }
          | return Expr ';'                          { Return $2 }

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
     | IDENT '=' Expr                                { Assign $1 $3 }
     | IDENT                                         { Ref $1 }
     | LIT                                           { Lit64 $1 }

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

parse :: String -> Either String [Defn]
parse s = expr (tokenize s) "foo" 1
}
