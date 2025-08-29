{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.LexerSpec where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Ast
import Language.Rosa.Monad
import Language.Rosa.Parser
import Language.Rosa.SourceFile

import Test.Tasty
import Test.Tasty.HUnit

tokens :: BL.ByteString -> [Token]
tokens input =
  case runRosa (runParser tokenize (StdinSource input)) of
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
      tokens "( )"
        @?= [ (Span 1 1 1 2, TokSymbol "(")
            , (Span 1 3 1 4, TokSymbol ")")
            ]

  , testCase "tokenize keywords" $
      tokens "import"
        @?= [ (Span 1 1 1 7, TokKeyword "import")
            ]

  , testCase "tokenize 'bool' literals" $
      tokens "false true"
        @?= [ (Span 1 1 1 6, TokBool False)
            , (Span 1 7 1 11, TokBool True)
            ]

  , testCase "tokenize 'int' literals (base 2)" $
      tokens "0b0 0b101 0b001100 0b000000"
        @?= [ (Span 1 1 1 4, TokInt 0)
            , (Span 1 5 1 10, TokInt 5)
            , (Span 1 11 1 19, TokInt 12)
            , (Span 1 20 1 28, TokInt 0)
            ]

  , testCase "tokenize 'int' literals (base 8)" $
      tokens "0o0 0o123 0o0456 0o007700 0o0000000"
        @?= [ (Span 1 1 1 4, TokInt 0)
            , (Span 1 5 1 10, TokInt 83)
            , (Span 1 11 1 17, TokInt 302)
            , (Span 1 18 1 26, TokInt 4032)
            , (Span 1 27 1 36, TokInt 0)
            ]

  , testCase "tokenize 'int' literals (base 10)" $
      tokens "0 123 0456 0078900 0000000"
        @?= [ (Span 1 1 1 2, TokInt 0)
            , (Span 1 3 1 6, TokInt 123)
            , (Span 1 7 1 11, TokInt 456)
            , (Span 1 12 1 19, TokInt 78900)
            , (Span 1 20 1 27, TokInt 0)
            ]

  , testCase "tokenize 'int' literals (base 16)" $
      tokens "0x0 0x123 0x0456 0x0078900 0x0000000"
        @?= [ (Span 1 1 1 4, TokInt 0)
            , (Span 1 5 1 10, TokInt 291)
            , (Span 1 11 1 17, TokInt 1110)
            , (Span 1 18 1 27, TokInt 493824)
            , (Span 1 28 1 37, TokInt 0)
            ]

  , testCase "tokenize identifiers" $
      tokens "lorem ipsum-dolor"
        @?= [ (Span 1 1 1 6, TokIdent "lorem")
            , (Span 1 7 1 18, TokIdent "ipsum-dolor")
            ]
  ]
