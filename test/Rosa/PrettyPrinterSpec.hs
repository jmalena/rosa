module Rosa.PrettyPrinterSpec where

import Rosa.AST
import Rosa.Parser
import Rosa.Parser.Gen
import Rosa.PrettyPrinter

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "PrettyPrinter" $
    it "`&&` should have precedence over `||`" $ do
      (prettyPrint [Func "main" [BlockStmt $ Return (BinaryOp OpLogOr (BinaryOp OpLogAnd (Lit64 1) (Lit64 0)) (Lit64 1))]]
       `shouldBe`
       "int main() {\n" <>
       "  return (1 && 0) || 1;\n" <>
       "}\n\n")
      (prettyPrint [Func "main" [BlockStmt $ Return (BinaryOp OpLogAnd (BinaryOp OpLogOr (Lit64 1) (Lit64 0)) (Lit64 1))]]
       `shouldBe`
       "int main() {\n" <>
       "  return 1 || 0 && 1;\n" <>
       "}\n\n")
      (prettyPrint [Func "main" [BlockStmt $ Return (BinaryOp OpLogOr (Lit64 1) (BinaryOp OpLogAnd (Lit64 0) (Lit64 1)))]]
       `shouldBe`
       "int main() {\n" <>
       "  return 1 || (0 && 1);\n" <>
       "}\n\n")
      (prettyPrint [Func "main" [BlockStmt $ Return (BinaryOp OpLogAnd (Lit64 1) (BinaryOp OpLogOr (Lit64 0) (Lit64 1)))]]
       `shouldBe`
       "int main() {\n" <>
       "  return 1 && (0 || 1);\n" <>
       "}\n\n")
