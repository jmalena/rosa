{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative

import Rosa.Compiler

import System.Process

data Options = Options
  { inputFile :: String
  , outputFile :: String
  }

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "ROSA, C language compiler." )
    options = Options
      <$> argument str
          ( metavar "INPUT"
         <> help "Where to load C source code file." )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> showDefault
         <> value "./out"
         <> help "Where to put compiled executable." )

run :: Options -> IO ()
run (Options { inputFile, outputFile }) = do
  source <- readFile inputFile
  case compile source of
    Left err -> error err
    Right asm -> do
      let asmFile = outputFile <> ".s"
      writeFile asmFile asm
      runCommand $ "gcc " <> asmFile <> " -o '" <> outputFile <> "'"
      return ()
