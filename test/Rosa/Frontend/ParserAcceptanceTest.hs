{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Rosa.Frontend.ParserAcceptanceTest where

import Data.String.QQ

import Test.Tasty
import Test.Tasty.HUnit

import Rosa.Frontend.Parser (parse)

--------------------------------------------------------------------------------
-- | Function Declarations

unit_funcDecl :: IO ()
unit_funcDecl =
  assertValidProgram "int main();"

unit_funcDecl_woSemi :: IO ()
unit_funcDecl_woSemi =
  assertInvalidProgram "int main()"

unit_funcDecl_wParam :: IO ()
unit_funcDecl_wParam =
  assertValidProgram "int main(int param1);"

unit_funcDecl_wParam_woType :: IO ()
unit_funcDecl_wParam_woType =
  assertInvalidProgram "int main(param1);"

unit_funcDecl_wParams :: IO ()
unit_funcDecl_wParams =
  assertValidProgram "int main(int param1, int param2);"

--------------------------------------------------------------------------------
-- | Function Definitions

unit_funcDefn :: IO ()
unit_funcDefn =
  assertValidProgram "int main() {}"

unit_funcDefn_wSemi :: IO ()
unit_funcDefn_wSemi =
  assertInvalidProgram "int main() {};"

unit_funcDefn_woBlock :: IO ()
unit_funcDefn_woBlock =
  assertInvalidProgram "int main() 0"

--------------------------------------------------------------------------------
-- | Variable Declaractions

unit_varDecl :: IO ()
unit_varDecl =
  assertValidProgram [s|
    int main() {
      int a;
    }
  |]

unit_varDecl_woSemi :: IO ()
unit_varDecl_woSemi =
  assertInvalidProgram [s|
    int main() {
      int a
    }
  |]

unit_varDecl_multi_FEATURE :: IO ()
unit_varDecl_multi_FEATURE = pure ()
{-
  assertValidProgram [s|
    int main() {
      int a, b;
    }
  |]
-}

unit_varDecl_multiLine :: IO ()
unit_varDecl_multiLine =
  assertValidProgram [s|
    int main() {
      int a;
      int b;
    }
  |]

--------------------------------------------------------------------------------
-- | Variable Definitions

unit_varDefn :: IO ()
unit_varDefn =
  assertValidProgram [s|
    int main() {
      int a = 1;
    }
  |]

unit_varDefn_multi_FEATURE :: IO ()
unit_varDefn_multi_FEATURE = pure ()
{-
  assertValidProgram [s|
    int main() {
      int a, b = 1;
    }
  |]
-}

unit_varDefn_multiLine :: IO ()
unit_varDefn_multiLine =
  assertValidProgram [s|
    int main() {
      int a = 1;
      int b = 2;
    }
  |]

--------------------------------------------------------------------------------
-- | Expr Statements

unit_exprStmt :: IO ()
unit_exprStmt =
  assertValidProgram [s|
    int main() {
      0;
    }
  |]
  
--------------------------------------------------------------------------------
-- | If-then Statements

unit_ifStmt_wEmptyExprStmt :: IO ()
unit_ifStmt_wEmptyExprStmt =
  assertValidProgram [s|
    int main() {
      if (1);
    }
  |]
  
unit_ifStmt_wExprStmt :: IO ()
unit_ifStmt_wExprStmt =
  assertValidProgram [s|
    int main() {
      if (1) 2;
    }
  |]
  
unit_ifStmt_wEmptyThenBlock :: IO ()
unit_ifStmt_wEmptyThenBlock =
  assertValidProgram [s|
    int main() {
      if (1) {}
    }
  |]

unit_ifStmt_woTestExpr :: IO ()
unit_ifStmt_woTestExpr =
  assertInvalidProgram [s|
    int main() {
      if () {}
    }
  |]
  
unit_ifStmt_woThen :: IO ()
unit_ifStmt_woThen =
  assertInvalidProgram [s|
    int main() {
      if (1)
    }
  |]

unit_ifStmt_wDeclInThen :: IO ()
unit_ifStmt_wDeclInThen =
  assertInvalidProgram [s|
    int main() {
      if (1) int x;
    }
  |]

unit_ifStmt_wDefnInThen :: IO ()
unit_ifStmt_wDefnInThen =
  assertInvalidProgram [s|
    int main() {
      if (1) int x = 2;
    }
  |]

--------------------------------------------------------------------------------
-- | If-then-else Statements

--------------------------------------------------------------------------------
-- | Test Utils

assertValidProgram :: String -> Assertion
assertValidProgram s =
  case parse s of
    (Right _) -> pure ()
    (Left e) ->
      assertFailure $ "Expect following string to be a valid program:\n\n  " ++ show s ++ "\n\nbut it got error:\n\n  " ++ show e

assertInvalidProgram :: String -> Assertion
assertInvalidProgram s =
  case parse s of
    (Left _) -> pure ()
    (Right ast) ->
      assertFailure $ "Expect following string to not be a valid program:\n\n  " ++ show s ++ "\n\nbut it got parsed as:\n\n  " ++ show ast
