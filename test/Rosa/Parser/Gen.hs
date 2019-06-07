module Rosa.Parser.Gen where

import Rosa.AST

import Test.QuickCheck
import Test.QuickCheck.Gen

genProgram :: Gen [Defn]
genProgram =
  (:[]) <$> (Func "main" <$> sequence [ Return <$> genArithmeticExpr64 ])

genArithmeticExpr64 :: Gen Expr
genArithmeticExpr64 = oneof
  [ UnaryOp <$> genUnOp <*> genExpr
  , BinaryOp <$> genBinOp <*> genExpr <*> genExpr
  ]
  where
    genExpr = oneof
      [ UnaryOp <$> genUnOp <*> genExpr
      , BinaryOp <$> genBinOp <*> genExpr <*> genExpr
      , Lit64 <$> arbitrary
      ]
    genUnOp = elements [ OpAddCompl ]
    genBinOp = elements [ OpAdd, OpSub {-, OpMul, OpDiv -} ]
