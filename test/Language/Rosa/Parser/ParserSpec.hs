module Language.Rosa.Parser.ParserSpec where

import qualified Data.ByteString.Lazy.Char8 as BL

import Language.Rosa.Core
import Language.Rosa.Monad
import Language.Rosa.Parser

import System.FilePath

import Test.Tasty
import Test.Tasty.Golden

rosaExt :: FilePath
rosaExt = ".rosa"

outExt :: FilePath
outExt = ".out"

testDir :: FilePath
testDir = "test" </> "golden" </> "parser"

goldenAstTest :: FilePath -> TestTree
goldenAstTest srcPath = goldenVsString srcFileName goldenPath runner
  where
    srcFileName = takeFileName srcPath
    goldenPath = testDir </> addExtension srcFileName outExt
    runner = do
      srcFile <- FileSource srcPath <$> BL.readFile srcPath
      pure $
        case runRosa (runParser parseModule srcFile) of
          Left err -> BL.pack $ show err
          Right _ -> BL.empty

tasty_parser :: IO TestTree
tasty_parser = do
  srcPaths <- findByExtension [rosaExt] testDir
  return $
    testGroup "Parser" $
      [ goldenAstTest srcPath | srcPath <- srcPaths ]
