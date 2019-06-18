module Rosa.Parser.Gen where

import Rosa.AST

import Test.QuickCheck
import Test.QuickCheck.Gen

genProgram :: Gen [BlockItem] -> Gen [Defn]
genProgram body =
  (:[]) <$> (Func "main" <$> body)

genExpr :: Gen Expr
genExpr = oneof
  [ UnaryOp <$> genUnOp <*> genExpr
  , BinaryOp <$> genBinOp <*> genExpr <*> genExpr
  ]
  where
    genExpr = oneof
      [ UnaryOp <$> genUnOp <*> genExpr
      , BinaryOp <$> genBinOp <*> genExpr <*> genExpr
      , genLit64
      ]
    genUnOp = elements [ OpAddCompl, OpLogCompl, OpBitCompl ]
    genBinOp = elements [ OpAdd, OpSub, OpMul, OpDiv, OpLTE, OpLT, OpGTE, OpGT, OpEQ, OpNEQ, OpLogAnd, OpLogOr ]

genLit64 :: Gen Expr
genLit64 = oneof
  [ Lit64 <$> arbitrary
  , UnaryOp OpAddCompl <$> (Lit64 <$> arbitrary)
  ]

genArithOpsExpr64 :: Gen Expr
genArithOpsExpr64 = oneof
  [ UnaryOp <$> genUnOp <*> genExpr
  , BinaryOp <$> genBinOp <*> genExpr <*> genExpr
  ]
  where
    genExpr = oneof
      [ UnaryOp <$> genUnOp <*> genExpr
      , BinaryOp <$> genBinOp <*> genExpr <*> genExpr
      , genLit64
      ]
    genUnOp = elements [ OpAddCompl ]
    genBinOp = elements [ OpAdd, OpSub, OpMul, OpDiv ]

genLogicOpsExpr64 :: Gen Expr
genLogicOpsExpr64 = oneof
  [ UnaryOp <$> genUnOp <*> genExpr
  , BinaryOp <$> genBinOp <*> genExpr <*> genExpr
  ]
  where
    genExpr = oneof
      [ UnaryOp <$> genUnOp <*> genExpr
      , BinaryOp <$> genBinOp <*> genExpr <*> genExpr
      , genLit64
      ]
    genUnOp = elements [ OpLogCompl ]
    genBinOp = elements [ OpLTE, OpLT, OpGTE, OpGT, OpEQ, OpNEQ, OpLogAnd, OpLogOr ]

genBitLogicOpsExpr64 :: Gen Expr
genBitLogicOpsExpr64 =
  UnaryOp <$> genUnOp <*> genExpr
  where
    genExpr = oneof
      [ UnaryOp <$> genUnOp <*> genExpr
      , genLit64
      ]
    genUnOp = elements [ OpBitCompl ]
