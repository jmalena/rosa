{-# LANGUAGE OverloadedStrings #-}

module Language.Rosa.Parser.ParserAstGolden where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Monad
import Language.Rosa.Parser
import Language.Rosa.SourceFile

import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

rosaExt :: FilePath
rosaExt = ".rosa"

astExt :: FilePath
astExt = ".ast"

goldenDir :: FilePath
goldenDir = "test" </> "golden"

testDir :: FilePath
testDir = goldenDir </> "parser"

goldenAstTest :: FilePath -> TestTree
goldenAstTest srcPath = goldenVsString srcFileName goldenPath runner
  where
    srcFileName = takeFileName srcPath
    goldenPath = testDir </> addExtension srcFileName astExt
    runner = do
      srcFile <- FileSource srcPath <$> BL.readFile srcPath
      case runRosa (runParser parseModule srcFile) of
        Left err -> error (show err)
        Right ast -> pure $ BL.pack (show ast)

tasty_parserAst :: IO TestTree
tasty_parserAst = do
  srcPaths <- findByExtension [rosaExt] testDir
  return $
    testGroup "Parser AST" $
      [ goldenAstTest srcPath | srcPath <- srcPaths ]
