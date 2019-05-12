module Main where

import Rosa.Compiler

main :: IO ()
main = do
  line <- getLine
  case compile line of
    Left err -> error err
    Right asm -> writeFile "out.s" asm
