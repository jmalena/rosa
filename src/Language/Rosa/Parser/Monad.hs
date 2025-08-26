module Language.Rosa.Parser.Monad where

import Language.Rosa.Error

data Parser a
  = ParserResult a
  | ParserFail RosaError

instance Functor Parser where
  fmap f (ParserResult x) = ParserResult (f x)
  fmap _ (ParserFail e) = ParserFail e

instance Applicative Parser where
  pure = ParserResult
  ParserResult f <*> ParserResult x =
    ParserResult (f x)
  ParserFail e <*> _ = ParserFail e
  _ <*> ParserFail e = ParserFail e

instance Monad Parser where
  return = pure
  ParserResult x >>= f = f x
  ParserFail e >>= _ = ParserFail e
