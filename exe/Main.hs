{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Applicative

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
     <> progDesc "Rosa Language Compiler." )
    options = Options
      <$> argument str
          ( metavar "INPUT"
         <> help "Path to the Rosa source file." )
      <*> strOption
          ( long "output"
         <> short 'o'
         <> showDefault
         <> value "./out"
         <> help "Path of the output executable." )

run :: Options -> IO ()
run (Options { inputFile, outputFile }) = do
  source <- readFile inputFile
  error "unimplemented"
