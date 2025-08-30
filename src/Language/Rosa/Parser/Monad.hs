{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Rosa.Parser.Monad where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Int

import Language.Rosa.Error
import Language.Rosa.Data.SourceSpan
import Language.Rosa.Monad
import Language.Rosa.SourceFile

type ParserInput = 
  ( SrcPos        -- ^ current position,
  , Char          -- ^ previous char
  , BL.ByteString -- ^ current input string
  , Int64         -- ^ bytes consumed so far
  )

data ParserState = ParserState
  { curInp  :: ParserInput -- ^ current parser input
  , curScd :: !Int         -- ^ current startcode
  }

newtype Parser a = Parser { unParser :: StateT ParserState Rosa a }
  deriving newtype
    (Functor, Applicative, Monad, MonadState ParserState, MonadError RosaError)

setInput :: ParserInput -> Parser ()
setInput inp = modify' $ \st -> st { curInp = inp }

getInput :: Parser ParserInput
getInput = gets curInp

setStartCode :: Int -> Parser ()
setStartCode sc = modify' $ \st -> st { curScd = sc }

getStartCode :: Parser Int
getStartCode = gets curScd

runParser :: Parser a -> SourceFile -> Rosa a
runParser (Parser m) srcFile = evalStateT m st
  where
    s = srcFileContent srcFile
    st = ParserState (mkPos 1 1, '\n', s, 0) 0
