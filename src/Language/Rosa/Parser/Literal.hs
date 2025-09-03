module Language.Rosa.Parser.Literal where

import Language.Rosa.Error
import Language.Rosa.Parser.Errors

readBase :: Integral a => Int -> String -> Either ParseError a
readBase base s
  | base < 2       = Left $ ParserInternalError "readBase: invalid base"
  | null digits    = Left $ ParserInternalError "readBase: empty input"
  | otherwise      = Right $ toNum digits
  where
    (digits, _) = span validDigit s

    validDigit c = digitValue c < base

    digitValue c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'z' = 10 + (fromEnum c - fromEnum 'a')
      | c >= 'A' && c <= 'Z' = 10 + (fromEnum c - fromEnum 'A')
      | otherwise            = base  -- mark invalid

    toNum = foldl (\acc c -> acc * fromIntegral base + fromIntegral (digitValue c)) 0
