{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Rosa.Parser.Monad where

import Language.Rosa.Error
import Control.Monad.Except

newtype Parser a = Parser { unParser :: Except RosaError a }
  deriving (Functor, Applicative, Monad, MonadError RosaError)

runParser :: Parser a -> Either RosaError a
runParser = runExcept . unParser
