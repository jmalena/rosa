{
module Language.Rosa.Parser.Lexer where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Internal   as BL (c2w)
import           Data.Int
import           Data.Maybe
import           Data.Word

import Language.Rosa.Ast
import Language.Rosa.Data.SourceSpan
import Language.Rosa.Error
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
  <0> $white+                             ;

  -- states
  <0> "|-"                                { begin blockcom }

  -- symbols
  <0> $symbol			          { tokenF TokSymbol }

  -- keywords
  <0> @keyword			          { tokenF TokKeyword }

  -- literals
  <0> "true"                              { token $ TokBool True }
  <0> "false"                             { token $ TokBool False }
  <0> "0b" $bindig+                       { tokenInt (parseBase 2 . BL.drop 2) }
  <0> "0o" $octdig+                       { tokenInt (parseBase 8 . BL.drop 2) }
  <0>      $digit+                        { tokenInt (parseBase 10) }
  <0> "0x" $hexdig+                       { tokenInt (parseBase 16 . BL.drop 2) }

  -- identifiers
  <0> @ident                              { tokenF TokIdent }

  -- module paths
  <0> @modulepath                         { tokenF TokModulePath }

  -- block comments
  <blockcom> "-|"                         { begin 0 }
  <blockcom> .			          ;

{
--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

type Action a = SrcSpan -> BL.ByteString -> Parser a

begin :: Int -> Action Token
begin sc _ _ = do
  setStartCode sc
  nextToken

token :: Tok -> Action Token
token t sp _ = pure (sp, t)

tokenF :: (BL.ByteString -> Tok) -> Action Token
tokenF f sp s = pure (sp, f s)

tokenInt :: (BL.ByteString -> Maybe Word64) -> Action Token
tokenInt f sp s =
  case f s of
    Nothing -> throwRosaError (IntParserInternalError s)
    Just num -> pure $ (sp, TokInt num)

parseBase :: (Num a, Integral a) => Int -> BL.ByteString -> Maybe a
parseBase base s
  | base < 2  = Nothing
  | otherwise =
      if BL.null digits
        then Nothing
        else Just (toNum digits)
  where
    (digits, rest) = BL.span validDigit s

    validDigit c = digitValue c < base

    digitValue c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'z' = 10 + (fromEnum c - fromEnum 'a')
      | c >= 'A' && c <= 'Z' = 10 + (fromEnum c - fromEnum 'A')
      | otherwise            = base  -- mark invalid

    toNum = BL.foldl' (\acc c -> acc * fromIntegral base + fromIntegral (digitValue c)) 0

--------------------------------------------------------------------------------
-- Lexer
--------------------------------------------------------------------------------

type AlexInput = ParserInput

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, c, _, _) = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (p, _, cs, n) =
  case BL.uncons cs of
    Nothing -> Nothing
    Just (c, cs') ->
      let b = BL.c2w c
          p' = advancePos p (BL.singleton c)
          n' = succ n
      in p' `seq` cs' `seq` n' `seq` Just (b, (p', c, cs', n'))

nextToken :: Parser Token
nextToken = do
  inp@(p, _, s, n) <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF ->
      pure (mkSpan p p, TokEOF)

    AlexError (p', _, _, _) ->
      throwRosaError $ UnexpectedCharacter p'

    AlexSkip inp' _ -> do
      setInput inp'
      nextToken

    AlexToken inp'@(_, _, _, n') _ act -> do
      let len = n'-n
      let match = BL.take len s
      let sp = mkSpanFromText p match
      setInput inp'
      act sp match

tokenize :: Parser [Token]
tokenize = do
  tok <- nextToken
  if snd tok == TokEOF
    then pure []
    else (tok :) <$> tokenize
}
