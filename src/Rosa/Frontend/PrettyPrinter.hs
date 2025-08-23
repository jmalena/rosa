{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rosa.Frontend.PrettyPrinter (
  prettyPrint
) where

import Control.Monad
import Control.Monad.State

import Data.List

import Rosa.Frontend.AST

data PrinterState = PrinterState
  { code :: String
  , indent :: Int
  } deriving (Show)

newtype Printer a = Printer { runPrinter :: State PrinterState a }
  deriving (Functor, Applicative, Monad, MonadState PrinterState)

prettyPrint :: [ExternDecl] -> String
prettyPrint = code . flip execState printerState . runPrinter . printExternDecls
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

printExternDecls :: [ExternDecl] -> Printer ()
printExternDecls decls =
  forM_ decls $ \decl -> do
    printExternDecl decl
    emitLine ""

printExternDecl :: ExternDecl -> Printer ()
printExternDecl (FuncDecl ident params) =
  emitLine $ "int " <> getRawIdent ident <> "(" <> showFuncParams params <> ");"
printExternDecl (FuncDefn ident params body) = do
  emitLine $ "int " <> getRawIdent ident <> "(" <> showFuncParams params <> ") {"
  pushScope
  mapM_ printBlockItem body
  popScope
  emitLine "}"

showFuncParams :: [Ident] -> String
showFuncParams =
  intercalate ", " . fmap (("int " <>) . getRawIdent)

printBlockItem :: BlockItem -> Printer ()
printBlockItem (VarDecl ident) =
  emitLine $ "int " <> getRawIdent ident <> ";"
printBlockItem (VarDefn ident expr) =
  emitLine $ "int " <> getRawIdent ident <> "=" <> showExpr expr <> ";"
printBlockItem (BlockStmt stmt) =
  printStmt stmt

printStmt :: Stmt -> Printer ()
printStmt (StmtExpr Nothing) =
  emitLine ";"
printStmt (StmtExpr (Just expr)) =
  emitLine $ showExpr expr <> ";"
printStmt (Compound cstmt) = do
  emitLine "{"
  pushScope
  mapM_ printBlockItem cstmt
  popScope
  emitLine "}"
printStmt (If testExpr thenStmt Nothing) = do
  emitLine $ "if (" <> showExpr testExpr <> ")"
  pushScope
  printStmt thenStmt
  popScope
printStmt (If testExpr thenStmt (Just elseStmt)) = do
  emitLine $ "if (" <> showExpr testExpr <> ")"
  pushScope
  printStmt thenStmt
  popScope
  emitLine "else"
  pushScope
  printStmt elseStmt
  popScope
printStmt (For preExprMaybe testExprMaybe postExprMaybe bodyStmt) = do
  emitLine $ mconcat
    [ "for ("
    , maybe "" (\preExpr -> showExpr preExpr <> " ") preExprMaybe
    , ";"
    , maybe "" (\testExpr -> showExpr testExpr <> " ") testExprMaybe
    , ";"
    , maybe "" (\postExpr -> showExpr postExpr <> " ") postExprMaybe
    , ")"
    ]
  printStmt bodyStmt
printStmt (While testExpr bodyStmt) = do
  emitLine $ "while (" <> showExpr testExpr <> ")"
  printStmt bodyStmt
printStmt (Do bodyStmt testExpr) = do
  emitLine "do"
  printStmt bodyStmt
  emitLine $ "while (" <> showExpr testExpr <> ");"
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
