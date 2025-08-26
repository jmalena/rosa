{
module Language.Rosa.Parser.Lexer (
  Token(..),
  tokenize
) where

import Data.Word
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
  $eol                            ;
  $white+                         ;
  \(                              { \_ -> TokLPar }
  \)                              { \_ -> TokRPar }
  \{                              { \_ -> TokLBra }
  \}                              { \_ -> TokRBra }
  \;                              { \_ -> TokSemi }
  \!                              { \_ -> TokNeg }
  \*                              { \_ -> TokMul }
  \/                              { \_ -> TokDiv }
  \+                              { \_ -> TokPlus }
  \-                              { \_ -> TokMinus }
  "<="                            { \_ -> TokLte }
  "<"                             { \_ -> TokLt }
  ">="                            { \_ -> TokGte }
  ">"                             { \_ -> TokGt }
  "=="                            { \_ -> TokEq }
  "!="                            { \_ -> TokNeq }
  "&&"                            { \_ -> TokAnd }
  "||"                            { \_ -> TokOr }
  \=                              { \_ -> TokAssign }
  "if"                            { \_ -> TokIf }
  "else"                          { \_ -> TokElse }
  "for"                           { \_ -> TokFor }
  "while"                         { \_ -> TokWhile }
  "do"                            { \_ -> TokDo }
  "break"                         { \_ -> TokBreak }
  "continue"                      { \_ -> TokContinue }
  "return"                        { \_ -> TokRet }
  "int"                           { \_ -> TokInt }
  $digit+                         { \s -> TokLit (read s) }
  [$alpha \_] [$alpha $digit \_]* { \s -> TokIdent s }
  \,                              { \_ -> TokComma }

{
data Token
  = TokLPar
  | TokRPar
  | TokLBra
  | TokRBra
  | TokSemi
  | TokNeg
  | TokMul
  | TokDiv
  | TokPlus
  | TokMinus
  | TokLte
  | TokLt
  | TokGte
  | TokGt
  | TokEq
  | TokNeq
  | TokAnd
  | TokOr
  | TokAssign
  | TokIf
  | TokElse
  | TokFor
  | TokWhile
  | TokDo
  | TokBreak
  | TokContinue
  | TokRet
  | TokInt
  | TokLit Word64
  | TokIdent String
  | TokComma
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = alexScanTokens
}
