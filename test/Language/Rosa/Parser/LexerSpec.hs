{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.LexerSpec where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Core
import Language.Rosa.Monad
import Language.Rosa.Parser

import Test.Tasty
import Test.Tasty.HUnit

tokens :: BL.ByteString -> [Token]
tokens input =
  case runRosa (runParser tokenize (StdinSource input)) of
    Left err -> error (show err)
    Right toks -> toks

tasty_lexer :: TestTree
tasty_lexer = testGroup "Lexer"
  [ testCase "ignore whitespaces" $
      tokens " \n \n \t\n\n\t  "
        @?= []

  , testCase "ignore comments" $ do
      tokens "-- single line comment"
        @?= []
      tokens "|-\nblock\ncomment\n-|"
        @?= []

  , testCase "tokenize symbols" $
      tokens "( )"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 2), TSymbol "(")
            , (mkSpan (mkPos 1 3) (mkPos 1 4), TSymbol ")")
            ]

  , testCase "tokenize keywords" $
      tokens "import"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 7), TKeyword "import")
            ]

  , testCase "tokenize 'bool' literals" $
      tokens "false true"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 6), TBool False)
            , (mkSpan (mkPos 1 7) (mkPos 1 11), TBool True)
            ]

  , testCase "tokenize 'int' literals (base 2)" $
      tokens "0b0 0b101 0b001100 0b000000"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 4), TInt 0)
            , (mkSpan (mkPos 1 5) (mkPos 1 10), TInt 5)
            , (mkSpan (mkPos 1 11) (mkPos 1 19), TInt 12)
            , (mkSpan (mkPos 1 20) (mkPos 1 28), TInt 0)
            ]

  , testCase "tokenize 'int' literals (base 8)" $
      tokens "0o0 0o123 0o0456 0o007700 0o0000000"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 4), TInt 0)
            , (mkSpan (mkPos 1 5) (mkPos 1 10), TInt 83)
            , (mkSpan (mkPos 1 11) (mkPos 1 17), TInt 302)
            , (mkSpan (mkPos 1 18) (mkPos 1 26), TInt 4032)
            , (mkSpan (mkPos 1 27) (mkPos 1 36), TInt 0)
            ]

  , testCase "tokenize 'int' literals (base 10)" $
      tokens "0 123 0456 0078900 0000000"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 2), TInt 0)
            , (mkSpan (mkPos 1 3) (mkPos 1 6), TInt 123)
            , (mkSpan (mkPos 1 7) (mkPos 1 11), TInt 456)
            , (mkSpan (mkPos 1 12) (mkPos 1 19), TInt 78900)
            , (mkSpan (mkPos 1 20) (mkPos 1 27), TInt 0)
            ]

  , testCase "tokenize 'int' literals (base 16)" $
      tokens "0x0 0x123 0x0456 0x0078900 0x0000000"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 4), TInt 0)
            , (mkSpan (mkPos 1 5) (mkPos 1 10), TInt 291)
            , (mkSpan (mkPos 1 11) (mkPos 1 17), TInt 1110)
            , (mkSpan (mkPos 1 18) (mkPos 1 27), TInt 493824)
            , (mkSpan (mkPos 1 28) (mkPos 1 37), TInt 0)
            ]

  , testCase "tokenize identifiers" $
      tokens "lorem ipsum-dolor"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 6), TIdent "lorem")
            , (mkSpan (mkPos 1 7) (mkPos 1 18), TIdent "ipsum-dolor")
            ]

  , testCase "tokenize module paths" $
      tokens "rosa.base"
        @?= [ (mkSpan (mkPos 1 1) (mkPos 1 10), TModulePath ["rosa", "base"])
            ]
  ]
