{
module Language.Rosa.Parser.Lexer where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe

import Language.Rosa.Ast
import Language.Rosa.Parser.Token
}

%wrapper "posn-bytestring"

tokens :-
  $white+                             ;

  -- comments
  "(*" ([^\*]|\*[^\/]|\*\n|\n)* "*)"  ;
  "--" [^\n]*                         ;

  -- symbols
  ";"                                 { tok Semicolon }

  -- operators
  "="                                 { tok $ Op Assign }

  -- keywords
  "let"                               { tok KeywordLet }

  -- literals
  "true"                              { tok $ LiteralBool True }
  "false"                             { tok $ LiteralBool False }

  "0b" [01]+                          { tokF (LiteralInt . parseBase 2 . BL.drop 2) }
  "0o" [0-7]+                         { tokF (LiteralInt . parseBase 8 . BL.drop 2) }
       [0-9]+                         { tokF (LiteralInt . parseBase 10) }
  "0x" [0-9a-f]+                      { tokF (LiteralInt . parseBase 16 . BL.drop 2) }

  -- identifiers
  [a-z][0-9a-z\-]*                    { tokF IdentifierKebabCase }

{
parseBase :: Num a => Int -> BL.ByteString -> a
parseBase base bs = go bs
  where
    go s =
        if BL.null digits then 0 else fromInteger (toNum digits)
      where
        (digits, _) = BL.span validDigit s
    validDigit c = digitValue c < base
    digitValue c
        | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
        | c >= 'a' && c <= 'z' = 10 + (fromEnum c - fromEnum 'a')
        | c >= 'A' && c <= 'Z' = 10 + (fromEnum c - fromEnum 'A')
        | otherwise            = base -- invalid marker
    toNum = BL.foldl' (\acc c -> acc * fromIntegral base + fromIntegral (digitValue c)) 0

tok :: TokenClass -> AlexPosn -> BL.ByteString -> Token
tok t p s = (t, posnToSpan p s)

tokF :: (BL.ByteString -> TokenClass) -> AlexPosn -> BL.ByteString -> Token
tokF f p s = (f s, posnToSpan p s)

posnToSpan :: AlexPosn -> BL.ByteString -> Span
posnToSpan p@(AlexPn _ sl sc) s = Span sl sc el ec
  where
    (el, ec) = posnOffset p s

posnOffset :: AlexPosn -> BL.ByteString -> (Int, Int)
posnOffset (AlexPn _ line col) s = (line + numNewlines, newCol)
  where
    numNewlines = length (BL.elemIndices '\n' s)
    newCol = case BL.elemIndices '\n' s of
      [] -> col + fromIntegral (BL.length s)
      indices -> fromIntegral (BL.length s) - fromIntegral (last indices)

tokenize :: BL.ByteString -> [Token]
tokenize s = go (alexStartPos, '\n', s, 0)
  where
    go inp@(pos, _, s', _) =
      case alexScan inp 0 of
        AlexEOF -> []
        AlexError ((AlexPn _ line column), _, _, _) ->
          error $ "lexical error at line " ++ show line ++ ", column " ++ show column
        AlexSkip inp' _ -> go inp'
        AlexToken inp' len act ->
          act pos (BL.take (fromIntegral len) s') : go inp'
}
