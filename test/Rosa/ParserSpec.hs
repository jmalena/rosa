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
  xdescribe "Parser" $ do
    prop "should restore generic pretty printed program AST" $
      forAll (genProgram $ sequence [BlockStmt . Return <$> genExpr]) $
      prop_doubleInverse

    prop "should restore arithmetic pretty printed program AST" $
      forAll (genProgram $ sequence [BlockStmt . Return <$> genArithOpsExpr64]) $
      prop_doubleInverse

    prop "should restore logic pretty printed program AST" $
      forAll (genProgram $ sequence [BlockStmt . Return <$> genLogicOpsExpr64]) $
      prop_doubleInverse

    prop "should restore bitwise logic pretty printed program AST" $
      forAll (genProgram $ sequence [BlockStmt . Return <$> genBitLogicOpsExpr64]) $
      prop_doubleInverse
