{
module Language.Rosa.Parser.Lexer where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Internal   as BL (c2w)
import           Data.Int
import           Data.Maybe
import           Data.Word

import Language.Rosa.Ast
import Language.Rosa.Data.Position
import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Token
import Language.Rosa.SourceFile
}

------------------------------------------------------------
-- Character sets
------------------------------------------------------------

$symbol     = [ \( \) ]

$digit      = [0-9]
$bindig     = [0-1]
$octdig     = [0-7]
$hexdig     = [0-9a-f]

------------------------------------------------------------
-- Regular expressions
------------------------------------------------------------

@keyword    = import

@ident      = [a-z][a-z0-9\-]*

@modulepath = @ident (\.@ident)*

------------------------------------------------------------
-- Tokens
------------------------------------------------------------
tokens :-
  -- white spaces
  <0> $white+                             { skip }

  -- states
  <0> "|-"                                { begin blockcom }

  -- symbols
  <0> $symbol			          { string TokSymbol }

  -- keywords
  <0> @keyword			          { string TokKeyword }

  -- block comment
  <0> "|-" ([^\*]|\*[^\/]|\*\n|\n)* "-|"  { skip }
  <0> "--" [^\n]*                         { skip }

  -- literals
  <0> "true"                              { token $ TokBool True }
  <0> "false"                             { token $ TokBool False }
  <0> "0b" $bindig+                       { string (TokInt . parseBase 2 . BL.drop 2) }
  <0> "0o" $octdig+                       { string (TokInt . parseBase 8 . BL.drop 2) }
  <0>      $digit+                        { string (TokInt . parseBase 10) }
  <0> "0x" $hexdig+                       { string (TokInt . parseBase 16 . BL.drop 2) }

  -- identifiers
  <0> @ident                              { string TokIdent }

  -- module paths
  <0> @modulepath                         { string TokModulePath }

  -- block comments
  <blockcom> "-|"                         { begin 0 }
  <blockcom> .			          { skip }

{
--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

type Action a = BL.ByteString -> Parser a

skip :: Action Token
skip _ = nextToken

token :: Tok -> Action Token
token t _ = pure (Span 0 0 0 0, t)

string :: (BL.ByteString -> Tok) -> Action Token
string f s = pure (Span 0 0 0 0, f s)

begin :: Int -> Action Token
begin sc _ = do
  setStartCode sc
  nextToken

{-
alexEOF :: Alex Token
alexEOF = return (NoSpan, TokEOF)

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
-}

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

type AlexInput = ParserInput

tokenEOF :: Token
tokenEOF = (Span 0 0 0 0, TokEOF)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (p, _, cs, n) =
  case BL.uncons cs of
    Nothing -> Nothing
    Just (c, cs') ->
      let b = BL.c2w c
          p' = movePos p c
          n' = succ n
      in p' `seq` cs' `seq` n' `seq` Just (b, (p', c, cs', n'))

movePos :: Position -> Char -> Position
movePos NoPos       _    = NoPos
movePos (Pos o r _) '\n' = Pos (succ o) (succ r) 1
movePos (Pos o r c) '\r' = Pos (succ o) r        c
movePos (Pos o r c) _    = Pos (succ o) r        (succ c)

nextToken :: Parser Token
nextToken = do
  inp@(_, _, _, n) <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF ->
      pure tokenEOF

    AlexError _ ->
      undefined -- TODO: add error

    AlexSkip inp' _ -> do
      setInput inp'
      nextToken

    AlexToken inp'@(_, _, s, n') _ act -> do
      let len = n'-n
      setInput inp'
      act s

tokenize :: Parser [Token]
tokenize = do
  tok <- nextToken
  if tok == tokenEOF
    then pure []
    else (tok :) <$> tokenize

--------------------------------------------------------------------------------
-- Literals
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
}
