{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Rosa.Codegen (
  runCodegen,
  codegen
) where

import Control.Monad.State
import Control.Monad.Writer

import Data.Int
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Rosa.AST
import qualified Rosa.Codegen.Frame as Frame

import Debug.Trace

type ASM = String

data CodegenState = CodegenState
  { functionSignatures :: Map.Map Ident [Ident]
  , frameStack :: [Frame.Frame]
  , labelCounter :: Int
  } deriving (Show)

newtype Codegen a = Codegen
  { unCodegen :: StateT CodegenState (Writer ASM) a
  } deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadWriter ASM)

runCodegen :: Codegen a -> String
runCodegen cg = execWriter (evalStateT (unCodegen cg) initState)
  where
    initState = CodegenState
      { functionSignatures = Map.empty
      , frameStack = []
      , labelCounter = 0
      }

--------------------------------------------------------------------------------
-- | Code emitting

emit :: Int -> String -> Codegen ()
emit indent instr = tell (replicate indent ' ' <> instr <> "\n")

genLabel :: Codegen String
genLabel = do
  n <- gets labelCounter
  modify $ \s -> s { labelCounter = succ n }
  return $ "label_" <> show n

--------------------------------------------------------------------------------
-- | Function handling

introduceFunction :: Ident -> [Ident] -> Codegen ()
introduceFunction ident params =
  modify $ \s -> s { functionSignatures = Map.insert ident params (functionSignatures s) }

findFunctionSignature :: Ident -> Codegen (Maybe [Ident])
findFunctionSignature ident =
  Map.lookup ident <$> gets functionSignatures

--------------------------------------------------------------------------------
-- | Frame handling

pushFrame :: Frame.Frame -> Codegen ()
pushFrame frame =
  modify $ \s -> s { frameStack = frame:(frameStack s) }

{-
getCurrentFrame :: Codegen Frame.Frame
getCurrentFrame =
  gets (head . frameStack)
-}

modifyCurrentFrame :: (Frame.Frame -> Frame.Frame) -> Codegen ()
modifyCurrentFrame f =
  modify $ \s@(CodegenState { frameStack = frame:frameStack' }) ->
    s { frameStack = (f frame):frameStack' }

stateCurrentFrame :: (Frame.Frame -> (a, Frame.Frame)) -> Codegen a
stateCurrentFrame f =
  state $ \s@(CodegenState { frameStack = frame:frameStack' }) ->
    (\nextFrame -> s { frameStack = nextFrame:frameStack' }) <$> f frame

findFrameByVar :: Ident -> Codegen (Maybe Frame.Frame)
findFrameByVar ident =
  find (\frame -> isJust $ Frame.findVarOffset ident frame) <$> gets frameStack

popFrame :: Codegen Frame.Frame
popFrame =
  state $ \s@(CodegenState { frameStack = frame:nextFrameStack }) ->
    ( frame
    , s { frameStack = nextFrameStack }
    )

getPrecedingFrames :: Frame.Frame -> Codegen [Frame.Frame]
getPrecedingFrames frame =
  tail . dropWhile (/= frame) <$> gets frameStack

findVarOffset :: Ident -> Codegen (Maybe Int64)
findVarOffset ident = do
  frameMaybe <- findFrameByVar ident
  forM frameMaybe $ \frame -> do
    let frameOffset = fromJust $ Frame.findVarOffset ident frame
    skipSize <- sum . map Frame.size <$> getPrecedingFrames frame
    return $ -skipSize + frameOffset

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

codegen :: [Defn] -> Codegen ()
codegen defns = do
  emit 0 $ ".global _main"
  emit 0 ""
  mapM_ emitDefn defns

emitDefn :: Defn -> Codegen ()
emitDefn (FuncDecl ident params Nothing) =
  findFunctionSignature ident >>= \case
    Just _ ->
      error $ "Error: function \"" <> getRawIdent ident <> "\" is already defined."
    Nothing ->
      introduceFunction ident params
emitDefn (FuncDecl ident params (Just body)) =
  findFunctionSignature ident >>= \case
    Just _ ->
      error $ "Error: function \"" <> getRawIdent ident <> "\" is already defined."
    Nothing -> do
      emit 0 $ "_" <> getRawIdent ident <> ":"
      pushFrame Frame.empty
      introduceFunction ident params
      forM_ (zip [0,8..] params) $ \(offset, varIdent) -> -- insert phantom function argument offsets
        modifyCurrentFrame $ Frame.markVar varIdent (offset + 16) -- skip saved return address (wtf, why not `+ 8`?!)
      stateCurrentFrame Frame.alloc64 -- alloc for old %rbp value
      emit 2 $ "pushq %rbp"
      emit 2 $ "movq %rsp, %rbp"
      mapM_ emitBlockItem body
      emit 2 $ "movq %rbp, %rsp"
      emit 2 $ "popq %rbp"
      modifyCurrentFrame Frame.dealloc64
      popFrame
      emit 2 "retq"
      emit 0 ""

emitBlockItem :: BlockItem -> Codegen ()
emitBlockItem (BlockDecl ident maybeExpr) = do
  offset <- stateCurrentFrame Frame.alloc64
  modifyCurrentFrame $ Frame.markVar ident offset
  case maybeExpr of
    Nothing ->
      emit 2 "pushq $0"
    Just expr -> do
      emitExpr "rax" expr
      emit 2 "pushq %rax"
emitBlockItem (BlockStmt stmt) =
  emitStmt stmt

emitStmt :: Stmt -> Codegen ()
emitStmt (SideEff maybeExpr) =
  forM_ maybeExpr $ \expr ->
    emitExpr "rax" expr
emitStmt (Compound stmt) = do
  pushFrame Frame.empty
  mapM_ emitBlockItem stmt
  frame <- popFrame
  emit 2 $ "addq $" <> show (Frame.size frame) <> ", %rsp" -- stack dealloc
  return ()
emitStmt (If condExpr thenStmt Nothing) = do
  endLabel <- genLabel
  -- emit 2 $ "pushq %rax"
  emitExpr "rax" condExpr
  emit 2 $ "cmpq $0, %rax"
  emit 2 $ "je " <> endLabel
  emitStmt thenStmt
  emit 0 $ endLabel <> ":"
  -- emit 2 $ "popq %rax"
emitStmt (If condExpr thenStmt (Just elseStmt)) = do
  elseLabel <- genLabel
  endLabel <- genLabel
  -- emit 2 $ "pushq %rax"
  emitExpr "rax" condExpr
  emit 2 $ "cmpq $0, %rax"
  emit 2 $ "je " <> elseLabel
  emitStmt thenStmt
  emit 2 $ "jmp " <> endLabel
  emit 0 $ elseLabel <> ":"
  emitStmt elseStmt
  emit 0 $ endLabel <> ":"
  -- emit 2 $ "popq %rax"
emitStmt (For preExprMaybe condExprMaybe postExprMaybe bodyStmt) = do
  loopLabel <- genLabel
  endLabel <- genLabel
  mapM_ (emitExpr "rax") preExprMaybe
  emit 0 $ loopLabel <> ":"
  forM_ condExprMaybe $ \condExpr -> do
    emitExpr "rax" condExpr
    emit 2 $ "cmpq $0, %rax"
    emit 2 $ "je " <> endLabel
  emitStmt bodyStmt
  mapM_ (emitExpr "rax") postExprMaybe
  emit 2 $ "jmp " <> loopLabel
  emit 0 $ endLabel <> ":"
emitStmt (While condExpr bodyStmt) = do
  loopLabel <- genLabel
  endLabel <- genLabel
  emit 0 $ loopLabel <> ":"
  emitExpr "rax" condExpr
  emit 2 $ "cmpq $0, %rax"
  emit 2 $ "je " <> endLabel
  emitStmt bodyStmt
  emit 2 $ "jmp " <> loopLabel
  emit 0 $ endLabel <> ":"
emitStmt (Do bodyStmt condExpr) = do
  loopLabel <- genLabel
  endLabel <- genLabel
  emit 0 $ loopLabel <> ":"
  emitStmt bodyStmt
  emitExpr "rax" condExpr
  emit 2 $ "cmpq $0, %rax"
  emit 2 $ "je " <> endLabel
  emit 2 $ "jmp " <> loopLabel
  emit 0 $ endLabel <> ":"
emitStmt Break =
  error "\"break\" statement is not implemented yet."
emitStmt Continue =
  error "\"continue\" statement is not implemented yet."
emitStmt (Return expr) =
  emitExpr "rax" expr

emitExpr :: String -> Expr -> Codegen ()
emitExpr reg (Lit64 val) =
  emit 2 $ "movq $" <> show val <> ", %" <> reg
emitExpr reg (FuncCall ident args) =
  findFunctionSignature ident >>= \case
    Nothing ->
      error $ "Error: unable to call undefined function \"" <> getRawIdent ident <> "\"."
    Just sig -> do
      when (length sig /= length args) $
        error $ "Error: function \"" <> getRawIdent ident <> "\" expect " <> show (length sig) <> " arguments, but " <> show (length args) <> " was given."
      let argsStackSize = 8 * length sig
      forM_ (reverse args) $ \argExpr -> do
        stateCurrentFrame Frame.alloc64
        emitExpr reg argExpr
        emit 2 $ "pushq %" <> reg
      emit 2 $ "callq _" <> getRawIdent ident
      replicateM_ (length sig) $ -- dealloc
        modifyCurrentFrame Frame.dealloc64
      emit 2 $ "addq $" <> show argsStackSize <> ", %rsp"
emitExpr reg (UnaryOp OpBitCompl expr) = do
  emitExpr reg expr
  emit 2 $ "not %" <> reg
emitExpr reg (UnaryOp OpLogCompl expr) = do
  emitExpr reg expr
  emit 2 $ "cmpq $0, %" <> reg
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "sete %al"
emitExpr reg (UnaryOp OpAddCompl expr) = do
  emitExpr reg expr
  emit 2 $ "neg %" <> reg
emitExpr reg (BinaryOp OpMul expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "pushq %rdx"
  emit 2 $ "mulq %rcx" -- modifies %rax (result low) and %rdx (result high)
  emit 2 $ "popq %rdx"
  -- emit 2 $ "popq %rcx"
{- see https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf -}
emitExpr reg (BinaryOp OpDiv expr1 expr2) = do
  error "64-bit division is not implemented yet."
emitExpr reg (BinaryOp OpAdd expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "addq %rcx, %" <> reg
  -- emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpSub expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "subq %" <> reg <> ", %rcx"
  emit 2 $ "movq %rcx, %" <> reg
  -- emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpEQ expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "sete %al"
  -- emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpNEQ expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setne %al"
  -- emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpLTE expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setle %al"
  -- emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpLT expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setl %al"
  -- emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpGTE expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setge %al"
  -- emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpGT expr1 expr2) = do
  -- emit 2 $ "pushq %rcx"
  emitExpr reg expr1
  emit 2 $ "pushq %" <> reg
  emitExpr reg expr2
  emit 2 $ "popq %rcx"
  emit 2 $ "cmpq %" <> reg <> ", %rcx"
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setg %al"
  -- emit 2 $ "popq %rcx"
emitExpr reg (BinaryOp OpLogAnd expr1 expr2) = do
  emitExpr reg expr1
  emit 2 $ "cmpq $0, %" <> reg
  label1 <- genLabel
  emit 2 $ "jne " <> label1
  label2 <- genLabel
  emit 2 $ "jmp " <> label2
  emit 0 $ label1 <> ":"
  emitExpr reg expr2
  emit 2 $ "cmpq $0, %" <> reg
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setne %al"
  emit 0 $ label2 <> ":"
emitExpr reg (BinaryOp OpLogOr expr1 expr2) = do
  emitExpr reg expr1
  emit 2 $ "cmpq $0, %" <> reg
  label1 <- genLabel
  emit 2 $ "je " <> label1
  emit 2 $ "movq $1, %" <> reg
  label2 <- genLabel
  emit 2 $ "jmp " <> label2
  emit 0 $ label1 <> ":"
  emitExpr reg expr2
  emit 2 $ "cmpq $0, %" <> reg
  emit 2 $ "movq $0, %" <> reg
  emit 2 $ "setne %al"
  emit 0 $ label2 <> ":"
emitExpr reg (Assign ident expr) =
  findVarOffset ident >>= \case
    Nothing ->
      error $ "Error: unable to assign value to undefined variable \"" <> getRawIdent ident <> "\"."
    Just memOffset -> do
      emitExpr reg expr
      emit 2 $ "movq %" <> reg <> ", " <> show memOffset <> "(%rbp)"
emitExpr reg (Ref ident) =
  findVarOffset ident >>= \case
    Nothing ->
      error $ "Error: unable to reference undefined variable \"" <> getRawIdent ident <> "\"."
    Just memOffset ->
      emit 2 $ "movq " <> show memOffset <> "(%rbp), %" <> reg
