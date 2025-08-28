{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Rosa.Error where

import Control.Monad.Except

class (Eq a, Show a) => CompilerError a where
  compilerError :: a -> IO ()

data RosaError = forall a. (Eq a, Show a, CompilerError a) => RosaError a

instance Show RosaError where
  show (RosaError e) = show e

throwRosaError :: (CompilerError a, MonadError RosaError m) => a -> m b
throwRosaError e = throwError (RosaError e)
