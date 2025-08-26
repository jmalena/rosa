{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.ParserTest where

import Debug.Trace

import Language.Rosa.Parser

import Test.Tasty
import Test.Tasty.HUnit

unit_tokenize :: Assertion
unit_tokenize = traceShowId $ scanTokens "let foo = false;"
