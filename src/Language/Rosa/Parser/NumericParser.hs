module Language.Rosa.Parser.NumericParser where

import qualified Data.ByteString.Lazy.Char8 as BL

parseBase :: Num a => Int -> BL.ByteString -> a
parseBase base bs = go bs
  where
    go s =
      if BL.null digits then 0 else fromInteger (toNum digits)
      where
        (digits, _) = BL.span validDigit s

    validDigit c = digitValue c < base

    digitValue c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'z' = 10 + (fromEnum c - fromEnum 'a')
      | c >= 'A' && c <= 'Z' = 10 + (fromEnum c - fromEnum 'A')
      | otherwise            = base  -- invalid marker

    toNum = BL.foldl' (\acc c -> acc * fromIntegral base + fromIntegral (digitValue c)) 0
