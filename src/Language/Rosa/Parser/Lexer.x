{
{-# LANGUAGE FlexibleContexts #-}

module Language.Rosa.Parser.Lexer where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe

import Language.Rosa.Ast
import Language.Rosa.Error
import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.NumericParser
import Language.Rosa.Parser.Token
import Language.Rosa.SourceFile
}

%wrapper "posn-bytestring"

tokens :-
  $white+                             ;

  -- comments
  "(*" ([^\*]|\*[^\/]|\*\n|\n)* "*)"  ;
  "--" [^\n]*                         ;

  -- symbols
  ";"                                 { basic Semicolon }
  "."                                 { basic Dot }

  -- operators
  "="                                 { basic $ Op Assign }

  -- keywords
  "import"                            { basic KeywordImport }
  "let"                               { basic KeywordLet }

  -- literals
  "true"                              { basic $ LiteralBool True }
  "false"                             { basic $ LiteralBool False }
  "0b" [01]+                          { useInput (LiteralInt . parseBase 2 . BL.drop 2) }
  "0o" [0-7]+                         { useInput (LiteralInt . parseBase 8 . BL.drop 2) }
       [0-9]+                         { useInput (LiteralInt . parseBase 10) }
  "0x" [0-9a-f]+                      { useInput (LiteralInt . parseBase 16 . BL.drop 2) }

  -- identifiers
  [a-z][0-9a-z\-]*                    { useInput IdentifierKebabCase }

{
basic :: TokenClass -> AlexPosn -> BL.ByteString -> Token
basic t p s = (t, posnToSpan p s)

useInput :: (BL.ByteString -> TokenClass) -> AlexPosn -> BL.ByteString -> Token
useInput f p s = (f s, posnToSpan p s)

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

scanTokens :: SourceFile -> Parser [Token]
scanTokens source = go (alexStartPos, '\n', srcFileContent source, 0)
  where
    go inp@(pos, _, s', _) =
      case alexScan inp 0 of
        AlexEOF ->
          return []

        AlexError (AlexPn _ line col, _, _, _) ->
          throwRosaError (LexicalError line col)

        AlexSkip inp' _ ->
          (go inp')

        AlexToken inp' len act -> do
          let lexeme = BL.take (fromIntegral len) s'
	  tok  <- pure (act pos lexeme)
          toks <- go inp'
          return (tok:toks)
}
