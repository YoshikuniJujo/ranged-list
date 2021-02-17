{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Control.Arrow
import Data.Foldable
import Data.List.Length
import Data.List.Range
import Data.Bits
import Data.Word
import Numeric

import Bit

main :: IO ()
main = do
	putStrLn $ takeWord64 sample1 `showHex` ""
	putStrLn $ takeWord64 sample2 `showHex` ""

bitsToWord, bitsToWordLE :: LengthL 64 Bit -> Word64
bitsToWord = foldl' (\w b -> w `shiftL` 1 .|. bitToNum b) 0
bitsToWordLE = foldl' (\w b -> w `shiftR` 1 .|. bitToNum b `shiftL` 63) 0

sample1, sample2 :: String
sample1 = "...*..*..*...........*...**********...*************............******"
sample2 = "...*..*..*...........*.."

takeL :: ListToLengthL n => [a] -> Either (RangeL 0 (n - 1) a) (LengthL n a)
takeL = right fst . splitL

takeL' :: (LoosenLMax 0 (n - 1) n, Unfoldr 0 n n, ListToLengthL n) => a -> [a] -> LengthL n a
takeL' d = either ((`fillL` d) . loosenLMax) fst . splitL

takeWord64, takeWord64LE :: String -> Word64
takeWord64 = bitsToWord . takeL' O . (boolToBit . (== '*') <$>)
takeWord64LE = bitsToWordLE . takeL' O . (boolToBit . (== '*') <$>)