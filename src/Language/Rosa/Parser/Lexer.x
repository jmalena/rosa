{
module Language.Rosa.Parser.Lexer where

import qualified Data.ByteString.Lazy.Char8 as BL

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

  -- identifiers
  [a-z][a-z0-9\-]*                    { tokString KebabIdentifier }

{
tok :: TokenClass -> AlexPosn -> BL.ByteString -> Token
tok tokClass p s = (tokClass, posnToSpan p s)

tokString :: (BL.ByteString -> TokenClass) -> AlexPosn -> BL.ByteString -> Token
tokString f p s = (f s, posnToSpan p s)

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
