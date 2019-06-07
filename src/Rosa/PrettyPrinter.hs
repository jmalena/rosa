{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rosa.PrettyPrinter (
  prettyPrint
) where

import Control.Monad.State

import Data.List

import Rosa.AST

data PrinterState = PrinterState
  { code :: String
  , indent :: Int
  } deriving (Show)

newtype Printer a = Printer { runPrinter :: State PrinterState a }
  deriving (Functor, Applicative, Monad, MonadState PrinterState)

prettyPrint :: [Defn] -> String
prettyPrint = code . flip execState printerState . runPrinter . printDefns
  where printerState = PrinterState { code = "", indent = 0 }

emitLine :: String -> Printer ()
emitLine s = do
  pad <- replicate <$> gets indent <*> pure ' '
  modify $ \rec -> rec { code = code rec <> pad <> s <> "\n" }

pushScope :: Printer ()
pushScope =
  modify $ \rec -> rec { indent = indent rec + 2}

popScope :: Printer ()
popScope =
  modify $ \rec -> rec { indent = indent rec - 2}

--------------------------------------------------------------------------------

printDefns :: [Defn] -> Printer ()
printDefns defns =
  forM_ defns $ \defn -> do
    printDefn defn
    emitLine ""

printDefn :: Defn -> Printer ()
printDefn (Func name body) = do
  emitLine $ "int " <> name <> "() {"
  pushScope
  mapM_ printStmt body
  popScope
  emitLine "}"

printStmt :: Stmt -> Printer ()
printStmt (Decl ident Nothing) =
  emitLine $ "int " <> ident <> ";"
printStmt (Decl ident (Just expr)) =
  emitLine $ "int " <> ident <> "=" <> showExpr expr <> ";"
printStmt (SideEff expr) =
  emitLine $ showExpr expr <> ";"
printStmt (Return expr) =
  emitLine $ "return " <> showExpr expr <> ";"

showExpr :: Expr -> String
showExpr (Lit64 num) =
  show num
showExpr (Ref ident) =
  ident
showExpr (Assign ident expr) =
  ident <> " = " <> showExpr expr
showExpr (UnaryOp op expr) =
  showUnaryOp op <> showExpr expr
showExpr (BinaryOp op lexpr rexpr) = mconcat
  [ if lNeedsBrackets then "(" else " "
  , showExpr lexpr
  , if lNeedsBrackets then ") " else " "
  , showBinaryOp op
  , if rNeedsBrackets then " (" else " "
  , showExpr rexpr
  , if rNeedsBrackets then ")" else ""
  ]
  where
    lNeedsBrackets =
      case lexpr of
        UnaryOp _ _ -> True
        _ -> False
    rNeedsBrackets =
      case rexpr of
        UnaryOp _ _ -> True
        BinaryOp _ _ _ -> True
        _ -> False

showUnaryOp :: UnaryOp -> String
showUnaryOp OpBitCompl = "~"
showUnaryOp OpLogCompl = "!"
showUnaryOp OpAddCompl = "-"

showBinaryOp :: BinaryOp -> String
showBinaryOp OpMul = "*"
showBinaryOp OpDiv = "/"
showBinaryOp OpAdd = "+"
showBinaryOp OpSub = "-"
showBinaryOp OpLTE = "<="
showBinaryOp OpLT = "<"
showBinaryOp OpGTE = ">="
showBinaryOp OpGT = ">"
showBinaryOp OpEQ = "=="
showBinaryOp OpNEQ = "!="
showBinaryOp OpLogAnd = "&&"
showBinaryOp OpLogOr = "||"
