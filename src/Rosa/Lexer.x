{
module Rosa.Lexer (
  Token(..),
  tokenize
) where
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
  "return"                      { \s -> TokenRetKeyword }
  "int"                         { \s -> TokenIntKeyword }
  $digit+                       { \s -> TokenInt (read s) }
  $alpha [$alpha $digit \_]*    { \s -> TokenIdent s }

{
data Token
  = TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenSemi
  | TokenRetKeyword
  | TokenIntKeyword
  | TokenInt Int
  | TokenIdent String
  deriving (Eq, Show)

tokenize :: String -> [Token]
tokenize = alexScanTokens
}
