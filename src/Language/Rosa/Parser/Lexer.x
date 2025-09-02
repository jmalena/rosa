{
module Language.Rosa.Parser.Lexer where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Internal   as BL (c2w)
import           Data.Int
import           Data.Maybe
import           Data.List
import           Data.Word

import Language.Rosa.Ast
import Language.Rosa.Data.ModulePath
import Language.Rosa.Data.SourceSpan
import Language.Rosa.Error
import Language.Rosa.Parser.Errors
import Language.Rosa.Parser.Literal
import Language.Rosa.Parser.Monad
import Language.Rosa.Parser.Token
import Language.Rosa.SourceFile
}

------------------------------------------------------------
-- Character sets
------------------------------------------------------------

$return     = [\r]
$linefeed   = [\n]

$symbol     = [\(\)]

$digit      = [0-9]
$bindig     = [0-1]
$octdig     = [0-7]
$hexdig     = [0-9a-f]

------------------------------------------------------------
-- Regular expressions
------------------------------------------------------------

@newline    = $return?$linefeed

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
  <0> "--"                                { begin linecom }
  <0> "|-"                                { begin blockcom }

  -- special
  <0> @symbol			          { tokenF TSymbol }
  <0> @keyword			          { tokenF TKeyword }

  -- literals
  <0> "true"                              { token $ TBool True }
  <0> "false"                             { token $ TBool False }
  <0> "0b" $bindig+                       { tokenInt (parseBase 2 . drop 2) }
  <0> "0o" $octdig+                       { tokenInt (parseBase 8 . drop 2) }
  <0>      $digit+                        { tokenInt (parseBase 10) }
  <0> "0x" $hexdig+                       { tokenInt (parseBase 16 . drop 2) }

  -- identifiers
  <0> @ident                              { tokenF TIdent }

  -- module paths
  <0> @modulepath                         { tokenF (TModulePath . parseModulePath) }

  -- line comments
  <linecom> @newline                      { begin 0 }
  <linecom> .			          ;

  -- block comments
  <blockcom> "-|"                         { begin 0 }
  <blockcom> @newline                     ;
  <blockcom> .			          ;

{
--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

type Action a = SrcSpan -> String -> Parser a

begin :: Int -> Action Token
begin sc _ _ = do
  setStartCode sc
  nextToken

token :: TokenClass -> Action Token
token t sp _ = pure (sp, t)

tokenF :: (String -> TokenClass) -> Action Token
tokenF f sp s = pure (sp, f s)

tokenInt :: (String -> Maybe Word64) -> Action Token
tokenInt f sp s =
  case f s of
    Nothing -> throwRosaError (IntParserInternalError s)
    Just num -> pure $ (sp, TInt num)

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
  inp@(p, _, bs, n) <- getInput
  sc <- getStartCode
  case alexScan inp sc of
    AlexEOF ->
      pure (mkSpan p p, TEof)

    AlexError _ ->
      throwRosaError $ UnexpectedChar p (BL.head bs)

    AlexSkip inp' _ -> do
      setInput inp'
      nextToken

    AlexToken inp'@(_, _, _, n') _ act -> do
      let len = n'-n
      let lexeme = BL.take len bs
      let sp = lexemeSpan p lexeme
      setInput inp'
      act sp (BL.unpack lexeme)

tokenize :: Parser [Token]
tokenize = do
  tok <- nextToken
  if snd tok == TEof
    then pure []
    else (tok :) <$> tokenize
}
