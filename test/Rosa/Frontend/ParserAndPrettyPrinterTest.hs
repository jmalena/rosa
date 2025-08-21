{-# LANGUAGE ScopedTypeVariables #-}

module Rosa.Frontend.ParserAndPrettyPrinterTest where

import Data.Either

import Rosa.Frontend.AST
import Rosa.Frontend.Parser
import Rosa.Frontend.PrettyPrinter

import Test.Tasty
import Test.Tasty.SmallCheck

scprop_parsePrettyPrintIdempotence :: [Defn] -> Bool
scprop_parsePrettyPrintIdempotence ast =
  case parse (prettyPrint ast) of
    Right ast' -> ast == ast'
    Left _     -> False

scprop_parsingValidInputSucceeds :: [Defn] -> Bool
scprop_parsingValidInputSucceeds =
  isRight . parse . prettyPrint
