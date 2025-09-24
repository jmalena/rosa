{
module Language.Rosa.Parser.Lexer where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Internal   as BL (c2w)
import           Data.Int
import           Data.Maybe
import           Data.List
import           Data.Word

import Language.Rosa.Ast
import Language.Rosa.Core
import Language.Rosa.Error
import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Literal
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Token
}

------------------------------------------------------------
-- Character sets
------------------------------------------------------------

$return     = [\r]
$linefeed   = [\n]

$digit      = [0-9]
$bindig     = [0-1]
$octdig     = [0-7]
$hexdig     = [0-9a-f]

------------------------------------------------------------
-- Regular expressions
------------------------------------------------------------

@newlines   = ($return?$linefeed)+

@symbol     = \( | \) | _ | : | \-> | := | \. | \\
@keyword    = use | let | in | Type

@ident      = [a-z][a-z0-9\-]*

@modulepath = @ident (\.@ident)*

------------------------------------------------------------
-- Tokens
------------------------------------------------------------
tokens :-
  -- white spaces
  <0> @newlines                           { token TNewlines }
  <0> $white+                             ;

  -- states
  <0> "--"                                { begin linecom skip }
  <0> "|-"                                { begin blockcom skip }

  -- special
  <0> @symbol			          { tokenF TSymbol }
  <0> @keyword			          { tokenF TKeyword }

  -- literals
  <0> "true"                              { token (TBool True) }
  <0> "false"                             { token (TBool False) }
  <0> "0b" $bindig+                       { tokenM (fmap TInt . expectEither . readBase 2 . drop 2) }
  <0> "0o" $octdig+                       { tokenM (fmap TInt . expectEither . readBase 8 . drop 2) }
  <0>      $digit+                        { tokenM (fmap TInt . expectEither . readBase 10) }
  <0> "0x" $hexdig+                       { tokenM (fmap TInt . expectEither . readBase 16 . drop 2) }

  -- identifiers
  <0> @ident                              { tokenF TIdent }

  -- module paths
  <0> @modulepath                         { tokenF (TModulePath . readModulePath) }

  -- line comments
  <linecom> @newlines                     { begin 0 (token TNewlines) }
  <linecom> .			          ;

  -- block comments
  <blockcom> "-|"                         { begin 0 skip }
  <blockcom> @newlines                    ;
  <blockcom> .			          ;

{
--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

type Action a = SrcSpan -> String -> Parser a

-- | Skip the current lexer match.
skip :: Action (Located Token)
skip _ _ = nextToken

-- | Sets the lexer start code and then performs the Alex action.
begin :: Int -> Action (Located Token) -> Action (Located Token)
begin sc act sp s = do
  setStartCode sc
  act sp s

-- | Lifts a token into an Alex action.
token :: Token -> Action (Located Token)
token = tokenM . const . pure

-- | Alex action mapping a lexer match into a 'Token'.
tokenF :: (String -> Token) -> Action (Located Token)
tokenF f = tokenM (pure . f)

-- | Alex action mapping a lexer match into a 'Token' using a parser action.
tokenM :: (String -> Parser Token) -> Action (Located Token)
tokenM f sp s = do
  tc <- f s
  pure $ mkAnn sp tc

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
          p' = advancePos p c
          n' = succ n
      in p' `seq` cs' `seq` n' `seq` Just (b, (p', c, cs', n'))

nextToken :: Parser (Located Token)
nextToken = do
  inp@(p, _, bs, _) <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF -> do
      let sp = mkSpan p p
      pure $ mkAnn sp TEof

    AlexError _ ->
      throwRosaError $ UnexpectedChar p (BL.head bs)

    AlexSkip inp' _ -> do
      setInput inp'
      nextToken

    AlexToken inp' len act ->
      let s = take len (BL.unpack bs)
	  sp = p `spanOver` s
      in do
        setInput inp'
	act sp s

tokenize :: Parser [Located Token]
tokenize = do
  tok <- nextToken
  if val tok == TEof
    then pure []
    else (tok :) <$> tokenize
}
