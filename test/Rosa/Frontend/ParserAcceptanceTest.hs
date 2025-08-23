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

unit_funcDef :: IO ()
unit_funcDef =
  assertValidProgram "int main() {}"

unit_funcDef_wSemi :: IO ()
unit_funcDef_wSemi =
  assertInvalidProgram "int main() {};"

unit_funcDef_woBlock :: IO ()
unit_funcDef_woBlock =
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

unit_multiVarDecl_FEATURE :: IO ()
unit_multiVarDecl_FEATURE = pure ()
{-
  assertValidProgram [s|
    int main() {
      int a, b;
    }
  |]
-}

unit_multiLineVarDecl :: IO ()
unit_multiLineVarDecl =
  assertValidProgram [s|
    int main() {
      int a;
      int b;
    }
  |]

--------------------------------------------------------------------------------
-- | Variable Definitions

unit_varDef :: IO ()
unit_varDef =
  assertValidProgram [s|
    int main() {
      int a = 1;
    }
  |]

unit_multiVarDef_FEATURE :: IO ()
unit_multiVarDef_FEATURE = pure ()
{-
  assertValidProgram [s|
    int main() {
      int a, b = 1;
    }
  |]
-}

unit_multiLineVarDef :: IO ()
unit_multiLineVarDef =
  assertValidProgram [s|
    int main() {
      int a = 1;
      int b = 2;
    }
  |]

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
