module Rosa.ParserSpec where

import Rosa.Parser
import Rosa.Parser.Gen
import Rosa.PrettyPrinter

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Parser" $
    prop "should restore pretty printed AST" $ forAll genProgram $ \ast ->
      case parse (prettyPrint ast) of
        Left err -> error err
        Right ast' ->
          ast' == ast
