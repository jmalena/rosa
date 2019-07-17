module Rosa.ParserAndPrettyPrinterTest where

import Data.Maybe

import Rosa.AST
import Rosa.Parser
import Rosa.PrettyPrinter

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC

test_parser :: [TestTree]
test_parser =
  [ localOption (6 :: SC.SmallCheckDepth) $ SC.testProperty "SC: parse ∘ prettyPrint = id (top decl)" $ \topDecls ->
      property_leftInverse topDecls
  , localOption (6 :: SC.SmallCheckDepth) $ SC.testProperty "SC: parse ∘ prettyPrint = id (func body)" $ \funcBody ->
      property_leftInverse [FuncDecl (fromJust (mkIdent "main")) [] funcBody]
  ]
  where
    property_leftInverse ast =
      case parse (prettyPrint ast) of
        Left err -> error err
        Right ast' ->
          ast == ast'
