{
module Rosa.Frontend.Lexer (
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
  \(                              { \_ -> TokenLParen }
  \)                              { \_ -> TokenRParen }
  \{                              { \_ -> TokenLBracket }
  \}                              { \_ -> TokenRBracket }
  \;                              { \_ -> TokenSemi }
  \~                              { \_ -> TokenBitCompl }
  \!                              { \_ -> TokenLogCompl }
  \*                              { \_ -> TokenMul }
  \/                              { \_ -> TokenDiv }
  \+                              { \_ -> TokenPlus }
  \-                              { \_ -> TokenMinus }
  "<="                            { \_ -> TokenLTE }
  "<"                             { \_ -> TokenLT }
  ">="                            { \_ -> TokenGTE }
  ">"                             { \_ -> TokenGT }
  "=="                            { \_ -> TokenEQ }
  "!="                            { \_ -> TokenNEQ }
  "&&"                            { \_ -> TokenLogAnd }
  "||"                            { \_ -> TokenLogOr }
  \=                              { \_ -> TokenAssign }
  "if"                            { \_ -> TokenIfKeyword }
  "else"                          { \_ -> TokenElseKeyword }
  "for"                           { \_ -> TokenFor }
  "while"                         { \_ -> TokenWhile }
  "do"                            { \_ -> TokenDo }
  "break"                         { \_ -> TokenBreak }
  "continue"                      { \_ -> TokenContinue }
  "return"                        { \_ -> TokenRetKeyword }
  "int"                           { \_ -> TokenIntKeyword }
  $digit+                         { \s -> TokenLit (read s) }
  [$alpha \_] [$alpha $digit \_]* { \s -> TokenIdent s }
  \,                              { \_ -> TokenComma }

{
data Token
  = TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenSemi
  | TokenBitCompl
  | TokenLogCompl
  | TokenMul
  | TokenDiv
  | TokenPlus
  | TokenMinus
  | TokenLTE
  | TokenLT
  | TokenGTE
  | TokenGT
  | TokenEQ
  | TokenNEQ
  | TokenLogAnd
  | TokenLogOr
  | TokenAssign
  | TokenIfKeyword
  | TokenElseKeyword
  | TokenFor
  | TokenWhile
  | TokenDo
  | TokenBreak
  | TokenContinue
  | TokenRetKeyword
  | TokenIntKeyword
  | TokenLit Word64
  | TokenIdent String
  | TokenComma
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = alexScanTokens
}
