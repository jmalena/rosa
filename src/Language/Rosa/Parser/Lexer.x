{
{-# LANGUAGE FlexibleContexts #-}

module Language.Rosa.Parser.Lexer where

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe

import Language.Rosa.Ast
import Language.Rosa.Error
import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Token
import Language.Rosa.SourceFile
}

%wrapper "posn-bytestring"

--------------------------------------------------------------------------------
-- Character sets
--------------------------------------------------------------------------------
$symbol = [ \. ]

@keyword = import
@identifier = [a-z][a-z0-9\-]*

tokens :-
  $white+                             ;
  $symbol			      { string TokSymbol }
  @keyword			      { string TokKeyword }

  -- comments
  "(*" ([^\*]|\*[^\/]|\*\n|\n)* "*)"  ;
  "--" [^\n]*                         ;

  -- literals
  "true"                              { token $ TokBool True }
  "false"                             { token $ TokBool False }
  "0b" [01]+                          { string (TokInt . parseBase 2 . BL.drop 2) }
  "0o" [0-7]+                         { string (TokInt . parseBase 8 . BL.drop 2) }
       [0-9]+                         { string (TokInt . parseBase 10) }
  "0x" [0-9a-f]+                      { string (TokInt . parseBase 16 . BL.drop 2) }

  -- identifiers
  @identifier                         { string TokIdent }

{
--------------------------------------------------------------------------------
-- Action helpers
--------------------------------------------------------------------------------

token :: Tok -> AlexPosn -> BL.ByteString -> Token
token t p s = (posnToSpan p s, t)

string :: (BL.ByteString -> Tok) -> AlexPosn -> BL.ByteString -> Token
string f p s = (posnToSpan p s, f s)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

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
      | otherwise            = base  -- invalid marker

    toNum = BL.foldl' (\acc c -> acc * fromIntegral base + fromIntegral (digitValue c)) 0

--------------------------------------------------------------------------------
-- Alex lexer
--------------------------------------------------------------------------------

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
scanTokens srcFile = go (alexStartPos, '\n', srcFileContent srcFile, 0)
  where
    go inp@(pos, _, s, _) =
      case alexScan inp 0 of
        AlexEOF ->
          return []

        AlexError (AlexPn _ line col, _, _, _) ->
          throwRosaError (LexicalError line col)

        AlexSkip inp' _ ->
          (go inp')

        AlexToken inp' len act -> do
          let lexeme = BL.take (fromIntegral len) s
	  tok  <- pure (act pos lexeme)
          toks <- go inp'
          return (tok:toks)
}
