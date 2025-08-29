{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Rosa.Monad where

import Control.Monad.Except

import Language.Rosa.Error

newtype Rosa a = Rosa { unRosa :: Except RosaError a }
  deriving (Functor, Applicative, Monad, MonadError RosaError)

runRosa :: Rosa a -> Either RosaError a
runRosa = runExcept . unRosa
