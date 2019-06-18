{
module Rosa.Lexer (
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
  $eol                          ;
  $white+                       ;
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \{                            { \s -> TokenLBracket }
  \}                            { \s -> TokenRBracket }
  \;                            { \s -> TokenSemi }
  \~                            { \s -> TokenBitCompl }
  \!                            { \s -> TokenLogCompl }
  \*                            { \s -> TokenMul }
  \/                            { \s -> TokenDiv }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  "<="                          { \s -> TokenLTE }
  "<"                           { \s -> TokenLT }
  ">="                          { \s -> TokenGTE }
  ">"                           { \s -> TokenGT }
  "=="                          { \s -> TokenEQ }
  "!="                          { \s -> TokenNEQ }
  "&&"                          { \s -> TokenLogAnd }
  "||"                          { \s -> TokenLogOr }
  \=                            { \s -> TokenAssign }
  "if"                          { \s -> TokenIfKeyword }
  "else"                        { \s -> TokenElseKeyword }
  "return"                      { \s -> TokenRetKeyword }
  "int"                         { \s -> TokenIntKeyword }
  $digit+                       { \s -> TokenLit (read s) }
  $alpha [$alpha $digit \_]*    { \s -> TokenIdent s }

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
  | TokenRetKeyword
  | TokenIntKeyword
  | TokenLit Word64
  | TokenIdent String
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = alexScanTokens
}
