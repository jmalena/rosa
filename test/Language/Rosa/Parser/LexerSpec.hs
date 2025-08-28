{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.LexerSpec where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Ast
import Language.Rosa.Parser
import Language.Rosa.SourceFile

import Test.Hspec

tokens :: BL.ByteString -> [Token]
tokens input =
  case runParser (scanTokens (StdinSource input)) of
    Left err -> error (show err)
    Right ts -> ts

spec :: Spec
spec = parallel $ do
  describe "Lexer" $ do
    it "tokenize whitespaces" $ do
      tokens " \n \n \t\n\n\t  "
        `shouldBe` []
    it "tokenize comments" $ do
      tokens "-- single line comment"
        `shouldBe` []
      tokens "(* multi\nline\ncomment *)"
        `shouldBe` []
    it "tokenize symbols" $ do
      tokens "."
        `shouldBe` [ (TokSymbol ".", Span 1 1 1 2)
                   ]
    it "tokenize keywords" $ do
      tokens "import"
        `shouldBe` [ (TokKeyword "import", Span 1 1 1 7)
                   ]
    it "tokenize 'bool' literals" $ do
      tokens "false true"
        `shouldBe` [ (TokBool False, Span 1 1 1 6)
                   , (TokBool True, Span 1 7 1 11)
                   ]
    it "tokenize 'int' literals (base 2)" $ do
      tokens "0b0 0b101 0b001100 0b000000"
        `shouldBe` [ (TokInt 0, Span 1 1 1 4)
                   , (TokInt 5, Span 1 5 1 10)
                   , (TokInt 12, Span 1 11 1 19)
                   , (TokInt 0, Span 1 20 1 28)
                   ]
    it "tokenize 'int' literals (base 8)" $ do
      tokens "0o0 0o123 0o0456 0o007700 0o0000000"
        `shouldBe` [ (TokInt 0, Span 1 1 1 4)
                   , (TokInt 83, Span 1 5 1 10)
                   , (TokInt 302, Span 1 11 1 17)
                   , (TokInt 4032, Span 1 18 1 26)
                   , (TokInt 0, Span 1 27 1 36)
                   ]
    it "tokenize 'int' literals (base 10)" $ do
      tokens "0 123 0456 0078900 0000000"
        `shouldBe` [ (TokInt 0, Span 1 1 1 2)
                   , (TokInt 123, Span 1 3 1 6)
                   , (TokInt 456, Span 1 7 1 11)
                   , (TokInt 78900, Span 1 12 1 19)
                   , (TokInt 0, Span 1 20 1 27)
                   ]
    it "tokenize 'int' literals (base 16)" $ do
      tokens "0x0 0x123 0x0456 0x0078900 0x0000000"
        `shouldBe` [ (TokInt 0, Span 1 1 1 4)
                   , (TokInt 291, Span 1 5 1 10)
                   , (TokInt 1110, Span 1 11 1 17)
                   , (TokInt 493824, Span 1 18 1 27)
                   , (TokInt 0, Span 1 28 1 37)
                   ]
    it "tokenize identifiers" $ do
      tokens "lorem ipsum-dolor"
        `shouldBe` [ (TokIdent "lorem", Span 1 1 1 6)
                   , (TokIdent "ipsum-dolor", Span 1 7 1 18)
                   ]
