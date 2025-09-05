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
goldenAstTest path = goldenVsString name outPath runner
  where
    name = takeFileName path
    outPath = testDir </> name <.> outExt
    runner = do
      inp <- FileInput path <$> BL.readFile path
      pure $
        case runRosa (runParser parseModule inp) of
          Left err -> BL.pack $ show err
          Right _ -> BL.empty

tasty_parser :: IO TestTree
tasty_parser = do
  paths <- findByExtension [rosaExt] testDir
  return $
    testGroup "Parser" $
      [ goldenAstTest p | p <- paths ]
