{-# LANGUAGE ScopedTypeVariables #-}

module Rosa.ParserAndPrettyPrinterTest where

import Data.Either

import Rosa.AST
import Rosa.Parser
import Rosa.PrettyPrinter

import Test.Tasty
import Test.Tasty.Discover
import Test.Tasty.SmallCheck

scprop_parsePrettyPrintIdempotence :: [Defn] -> Bool
scprop_parsePrettyPrintIdempotence ast =
  case parse (prettyPrint ast) of
    Right ast' -> ast == ast'
    Left _     -> False

scprop_parsingValidInputSucceeds :: [Defn] -> Bool
scprop_parsingValidInputSucceeds =
  isRight . parse . prettyPrint
