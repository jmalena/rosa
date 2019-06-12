module Rosa.ParserSpec where

import Rosa.AST
import Rosa.Parser
import Rosa.Parser.Gen
import Rosa.PrettyPrinter

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

prop_doubleInverse :: [Defn] -> Bool
prop_doubleInverse ast =
  case parse (prettyPrint ast) of
    Left err -> error err
    Right ast' ->
      ast' == ast

spec :: Spec
spec = do
  describe "Parser" $ do
    prop "should restore generic pretty printed program AST" $
      forAll (genProgram $ sequence [Return <$> genExpr]) $
      prop_doubleInverse

    prop "should restore arithmetic pretty printed program AST" $
      forAll (genProgram $ sequence [Return <$> genArithOpsExpr64]) $
      prop_doubleInverse

    prop "should restore logic pretty printed program AST" $
      forAll (genProgram $ sequence [Return <$>  genLogicOpsExpr64]) $
      prop_doubleInverse

    prop "should restore bitwise logic pretty printed program AST" $
      forAll (genProgram $ sequence [Return <$> genBitLogicOpsExpr64]) $
      prop_doubleInverse
