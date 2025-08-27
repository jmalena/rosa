{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.LexerSpec where

import Language.Rosa.Ast
import Language.Rosa.Parser

import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "Lexer" $ do
    it "tokenize whitespaces" $ do
      tokenize " \n \n \t\n\n\t  "
        `shouldBe` []
    it "tokenize comments" $ do
      tokenize "-- single line comment"
        `shouldBe` []
      tokenize "(* multi\nline\ncomment *)"
        `shouldBe` []
    it "tokenize symbols" $ do
      tokenize ";"
        `shouldBe` [ (Semicolon, Span 1 1 1 2)
                   ]
    it "tokenize operators" $ do
      tokenize "="
        `shouldBe` [ (Op Assign, Span 1 1 1 2)
                   ]
    it "tokenize keywords" $ do
      tokenize "let"
        `shouldBe` [ (KeywordLet, Span 1 1 1 4)
                   ]
    it "tokenize 'bool' literals" $ do
      tokenize "false true"
        `shouldBe` [ (LiteralBool False, Span 1 1 1 6)
                   , (LiteralBool True, Span 1 7 1 11)
                   ]
    it "tokenize 'int' literals (base 2)" $ do
      tokenize "0b0 0b101 0b001100 0b000000"
        `shouldBe` [ (LiteralInt 0, Span 1 1 1 4)
                   , (LiteralInt 5, Span 1 5 1 10)
                   , (LiteralInt 12, Span 1 11 1 19)
                   , (LiteralInt 0, Span 1 20 1 28)
                   ]
    it "tokenize 'int' literals (base 8)" $ do
      tokenize "0o0 0o123 0o0456 0o007700 0o0000000"
        `shouldBe` [ (LiteralInt 0, Span 1 1 1 4)
                   , (LiteralInt 83, Span 1 5 1 10)
                   , (LiteralInt 302, Span 1 11 1 17)
                   , (LiteralInt 4032, Span 1 18 1 26)
                   , (LiteralInt 0, Span 1 27 1 36)
                   ]
    it "tokenize 'int' literals (base 10)" $ do
      tokenize "0 123 0456 0078900 0000000"
        `shouldBe` [ (LiteralInt 0, Span 1 1 1 2)
                   , (LiteralInt 123, Span 1 3 1 6)
                   , (LiteralInt 456, Span 1 7 1 11)
                   , (LiteralInt 78900, Span 1 12 1 19)
                   , (LiteralInt 0, Span 1 20 1 27)
                   ]
    it "tokenize 'int' literals (base 16)" $ do
      tokenize "0x0 0x123 0x0456 0x0078900 0x0000000"
        `shouldBe` [ (LiteralInt 0, Span 1 1 1 4)
                   , (LiteralInt 291, Span 1 5 1 10)
                   , (LiteralInt 1110, Span 1 11 1 17)
                   , (LiteralInt 493824, Span 1 18 1 27)
                   , (LiteralInt 0, Span 1 28 1 37)
                   ]
    it "tokenize kebab-case identifiers" $ do
      tokenize "lorem\nipsum-dolor"
        `shouldBe` [ (IdentifierKebabCase "lorem", Span 1 1 1 6)
                   , (IdentifierKebabCase "ipsum-dolor", Span 2 1 2 12)
                   ]
