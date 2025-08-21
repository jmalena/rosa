{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rosa.Frontend.PrettyPrinter (
  prettyPrint
) where

import Control.Monad.State

import Data.List

import Rosa.Frontend.AST

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
printDefn (FuncDecl ident params Nothing) =
  emitLine $ "int " <> getRawIdent ident <> "(" <> showFuncParams params <> ");"
printDefn (FuncDecl ident params (Just body)) = do
  emitLine $ "int " <> getRawIdent ident <> "(" <> showFuncParams params <> ") {"
  pushScope
  mapM_ printBlockItem body
  popScope
  emitLine "}"

showFuncParams :: [Ident] -> String
showFuncParams =
  intercalate ", " . fmap (("int " <>) . getRawIdent)

printBlockItem :: BlockItem -> Printer ()
printBlockItem (BlockDecl ident Nothing) =
  emitLine $ "int " <> getRawIdent ident <> ";"
printBlockItem (BlockDecl ident (Just expr)) =
  emitLine $ "int " <> getRawIdent ident <> "=" <> showExpr expr <> ";"
printBlockItem (BlockStmt stmt) =
  printStmt stmt

printStmt :: Stmt -> Printer ()
printStmt (SideEff Nothing) =
  emitLine ";"
printStmt (SideEff (Just expr)) =
  emitLine $ showExpr expr <> ";"
printStmt (Compound stmt) = do
  emitLine "{"
  pushScope
  mapM_ printBlockItem stmt
  popScope
  emitLine "}"
printStmt (If condExpr thenStmt Nothing) = do
  emitLine $ "if (" <> showExpr condExpr <> ")"
  pushScope
  printStmt thenStmt
  popScope
printStmt (If condExpr thenStmt (Just elseStmt)) = do
  emitLine $ "if (" <> showExpr condExpr <> ")"
  pushScope
  printStmt thenStmt
  popScope
  emitLine "else"
  pushScope
  printStmt elseStmt
  popScope
printStmt (For preExprMaybe condExprMaybe postExprMaybe bodyStmt) = do
  emitLine $ mconcat
    [ "for ("
    , maybe "" (\preExpr -> showExpr preExpr <> " ") preExprMaybe
    , ";"
    , maybe "" (\condExpr -> showExpr condExpr <> " ") condExprMaybe
    , ";"
    , maybe "" (\postExpr -> showExpr postExpr <> " ") postExprMaybe
    , ")"
    ]
  printStmt bodyStmt
printStmt (While condExpr bodyStmt) = do
  emitLine $ "while (" <> showExpr condExpr <> ")"
  printStmt bodyStmt
printStmt (Do bodyStmt condExpr) = do
  emitLine "do"
  printStmt bodyStmt
  emitLine $ "while (" <> showExpr condExpr <> ");"
printStmt Break =
  emitLine "break;"
printStmt Continue =
  emitLine "continue;"
printStmt (Return expr) =
  emitLine $ "return " <> showExpr expr <> ";"

showExpr :: Expr -> String
showExpr (NumLit num) =
  show num
showExpr (Ref ident) =
  getRawIdent ident
showExpr (Assign ident expr) =
  getRawIdent ident <> " = " <> showExpr expr
showExpr (FuncCall ident expr) =
  getRawIdent ident <> "(" <> intercalate ", " (showExpr <$> expr) <> ")"
showExpr (UnaryOp op expr) =
  showUnaryOp op <> showExpr expr
showExpr (BinaryOp op lexpr rexpr) = mconcat
  [ if lNeedsBrackets then "(" else ""
  , showExpr lexpr
  , if lNeedsBrackets then ") " else " "
  , showBinaryOp op
  , if rNeedsBrackets then " (" else " "
  , showExpr rexpr
  , if rNeedsBrackets then ")" else ""
  ]
  where
    lNeedsBrackets =
      case (op, lexpr) of
        (_, UnaryOp op' _) -> op `precOver` op'
        (_, BinaryOp op' _ _) -> op `precOver` op'
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
