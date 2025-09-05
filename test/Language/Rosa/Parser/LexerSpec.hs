module Language.Rosa.Parser.LexerSpec where

import Language.Rosa.Core
import Language.Rosa.Monad
import Language.Rosa.Parser

import Test.Tasty
import Test.Tasty.HUnit

runLex :: String -> [Located Token]
runLex s =
  case runRosa (runParser tokenize (InlineInput s)) of
    Left err -> error (show err)
    Right toks -> toks

tasty_lexer :: TestTree
tasty_lexer = testGroup "Lexer"
  [ testCase "ignore whitespaces" $
      runLex " \n \n \t\n\n\t  "
        @?= []

  , testCase "ignore comments" $ do
      runLex "-- single line comment"
        @?= []
      runLex "|-\nblock\ncomment\n-|"
        @?= []

  , testCase "tokenize symbols" $
      runLex "( ) _ : -> :="
        @?= [ 1.:.1 .-. 1.:.2 @: TSymbol "("
            , 1.:.3 .-. 1.:.4 @: TSymbol ")"
            , 1.:.5 .-. 1.:.6 @: TSymbol "_"
            , 1.:.7 .-. 1.:.8 @: TSymbol ":"
            , 1.:.9 .-. 1.:.11 @: TSymbol "->"
            , 1.:.12 .-. 1.:.14 @: TSymbol ":="
            ]

  , testCase "tokenize keywords" $
      runLex "use"
        @?= [ 1.:.1 .-. 1.:.4 @: TKeyword "use"
            ]

  , testCase "tokenize 'bool' literals" $
      runLex "false true"
        @?= [ 1.:.1 .-. 1.:.6 @: TBool False
            , 1.:.7 .-. 1.:.11 @: TBool True
            ]

  , testCase "tokenize 'int' literals (base 2)" $
      runLex "0b0 0b101 0b001100 0b000000"
        @?= [ 1.:.1 .-. 1.:.4  @: TInt 0
            , 1.:.5 .-. 1.:.10 @: TInt 5
            , 1.:.11 .-. 1.:.19 @: TInt 12
            , 1.:.20 .-. 1.:.28 @: TInt 0
            ]

  , testCase "tokenize 'int' literals (base 8)" $
      runLex "0o0 0o123 0o0456 0o007700 0o0000000"
        @?= [ 1.:.1 .-. 1.:.4   @: TInt 0
            , 1.:.5 .-. 1.:.10  @: TInt 83
            , 1.:.11 .-. 1.:.17 @: TInt 302
            , 1.:.18 .-. 1.:.26 @: TInt 4032
            , 1.:.27 .-. 1.:.36 @: TInt 0
            ]

  , testCase "tokenize 'int' literals (base 10)" $
      runLex "0 123 0456 0078900 0000000"
        @?= [ 1.:.1  .-. 1.:.2  @: TInt 0
            , 1.:.3  .-. 1.:.6  @: TInt 123
            , 1.:.7  .-. 1.:.11 @: TInt 456
            , 1.:.12 .-. 1.:.19 @: TInt 78900
            , 1.:.20 .-. 1.:.27 @: TInt 0
            ]

  , testCase "tokenize 'int' literals (base 16)" $
      runLex "0x0 0x123 0x0456 0x0078900 0x0000000"
        @?= [ 1.:.1  .-. 1.:.4   @: TInt 0
            , 1.:.5  .-. 1.:.10  @: TInt 291
            , 1.:.11 .-. 1.:.17 @: TInt 1110
            , 1.:.18 .-. 1.:.27 @: TInt 493824
            , 1.:.28 .-. 1.:.37 @: TInt 0
            ]

  , testCase "tokenize identifiers" $
      runLex "lorem ipsum-dolor"
        @?= [ 1.:.1  .-. 1.:.6  @: TIdent "lorem"
            , 1.:.7  .-. 1.:.18 @: TIdent "ipsum-dolor"
            ]

  , testCase "tokenize module paths" $
      runLex "rosa.base"
        @?= [ 1.:.1 .-. 1.:.10 @: TModulePath ["rosa", "base"]
            ]
  ]
