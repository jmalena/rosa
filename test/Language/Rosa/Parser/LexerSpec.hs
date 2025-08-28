{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.LexerSpec where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Ast
import Language.Rosa.Parser
import Language.Rosa.SourceFile

import Test.Tasty
import Test.Tasty.HUnit

tokens :: BL.ByteString -> [Token]
tokens input =
  case runParser (scanTokens (StdinSource input)) of
    Left err -> error (show err)
    Right toks -> toks

tasty_lexer :: TestTree
tasty_lexer = testGroup "Lexer"
  [ testCase "tokenize whitespaces" $
      tokens " \n \n \t\n\n\t  "
        @?= []

  , testCase "tokenize comments" $ do
      tokens "-- single line comment"
        @?= []
      tokens "(* multi\nline\ncomment *)"
        @?= []

  , testCase "tokenize symbols" $
      tokens "."
        @?= [ (TokSymbol ".", Span 1 1 1 2)
            ]

  , testCase "tokenize keywords" $
      tokens "import"
        @?= [ (TokKeyword "import", Span 1 1 1 7)
            ]

  , testCase "tokenize boolean literals" $
      tokens "false true"
        @?= [ (TokBool False, Span 1 1 1 6)
            , (TokBool True, Span 1 7 1 11)
            ]

  , testCase "tokenize integer literals (base 2)" $
      tokens "0b0 0b101 0b001100 0b000000"
        @?= [ (TokInt 0, Span 1 1 1 4)
            , (TokInt 5, Span 1 5 1 10)
            , (TokInt 12, Span 1 11 1 19)
            , (TokInt 0, Span 1 20 1 28)
            ]

  , testCase "tokenize integer literals (base 8)" $
      tokens "0o0 0o123 0o0456 0o007700 0o0000000"
        @?= [ (TokInt 0, Span 1 1 1 4)
            , (TokInt 83, Span 1 5 1 10)
            , (TokInt 302, Span 1 11 1 17)
            , (TokInt 4032, Span 1 18 1 26)
            , (TokInt 0, Span 1 27 1 36)
            ]

  , testCase "tokenize integer literals (base 10)" $
      tokens "0 123 0456 0078900 0000000"
        @?= [ (TokInt 0, Span 1 1 1 2)
            , (TokInt 123, Span 1 3 1 6)
            , (TokInt 456, Span 1 7 1 11)
            , (TokInt 78900, Span 1 12 1 19)
            , (TokInt 0, Span 1 20 1 27)
            ]

  , testCase "tokenize integer literals (base 16)" $
      tokens "0x0 0x123 0x0456 0x0078900 0x0000000"
        @?= [ (TokInt 0, Span 1 1 1 4)
            , (TokInt 291, Span 1 5 1 10)
            , (TokInt 1110, Span 1 11 1 17)
            , (TokInt 493824, Span 1 18 1 27)
            , (TokInt 0, Span 1 28 1 37)
            ]

  , testCase "tokenize identifiers" $
      tokens "lorem ipsum-dolor"
        @?= [ (TokIdent "lorem", Span 1 1 1 6)
            , (TokIdent "ipsum-dolor", Span 1 7 1 18)
            ]
  ]
