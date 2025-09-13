{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.LiteralSpec where

import Language.Rosa.Error
import Language.Rosa.Parser

import Test.Tasty
import Test.Tasty.HUnit

tasty_literal :: TestTree
tasty_literal = testGroup "Parser.Literal"
  [ testCase "binary literals" $ do
      readBase 2 "0"   @?= Right 0
      readBase 2 "1"   @?= Right 1
      readBase 2 "101" @?= Right 5

  , testCase "octal literals" $ do
      readBase 8 "0"   @?= Right 0
      readBase 8 "7"   @?= Right 7
      readBase 8 "123" @?= Right 83  -- 1*64 + 2*8 + 3 = 83

  , testCase "decimal literals" $ do
      readBase 10 "0"   @?= Right 0
      readBase 10 "9"   @?= Right 9
      readBase 10 "456" @?= Right 456

  , testCase "hexadecimal literals" $ do
      readBase 16 "0"    @?= Right 0
      readBase 16 "f"    @?= Right 15
      readBase 16 "1a3"  @?= Right 419
      readBase 16 "A1F"  @?= Right 2591

  , testCase "mixed case hex" $ do
      readBase 16 "aF"   @?= Right 175
      readBase 16 "B0c"  @?= Right 2828

  , testCase "invalid base" $ do
      case readBase 1 "101" of
        Left (ParserInternalError _) -> pure ()
        _ -> assertFailure "Expected error"

  , testCase "empty input" $ do
      case readBase 10 "" of
        Left (ParserInternalError _) -> pure ()
        _ -> assertFailure "Expected ParserInternalError"

  , testCase "invalid digits" $ do
      case readBase 2 "102" of
        Left (ParserInternalError _) -> pure ()
        _ -> assertFailure "Expected ParserInternalError"

      case readBase 8 "89" of
        Left (ParserInternalError _) -> pure ()
        _ -> assertFailure "Expected ParserInternalError"

  , testCase "invalid characters" $ do
      case readBase 2 " " of
        Left (ParserInternalError _) -> pure ()
        _ -> assertFailure "Expected ParserInternalError"

      case readBase 8 "$" of
        Left (ParserInternalError _) -> pure ()
        _ -> assertFailure "Expected ParserInternalError"
  ]
