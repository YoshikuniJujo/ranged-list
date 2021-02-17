{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Foldable
import Data.List.Length
import Data.List.Range
import Data.Bits
import Data.Word
import Numeric

main :: IO ()
main = do
	putStrLn $ takeWord64 sample1 `showHex` ""
	putStrLn $ takeWord64 sample2 `showHex` ""

bitsToWord :: LengthL 64 Bit -> Word64
bitsToWord = foldl' (\w b -> w `shiftR` 1 .|. bitToNum b `shiftL` 63) 0

takeL :: (LoosenLMax 0 (n - 1) n, Unfoldr 0 n n, ListToLengthL n) => a -> [a] -> LengthL n a
takeL d = either ((`fillL` d) . loosenLMax) fst . splitL

takeWord64 :: String -> Word64
takeWord64 = bitsToWord . takeL O . (boolToBit . (== '*') <$>)

data Bit = O | I deriving Show

bitToNum :: Num n => Bit -> n
bitToNum = \case O -> 0; I -> 1

boolToBit :: Bool -> Bit
boolToBit = \case False -> O; True -> I

sample1, sample2 :: String
sample1 = "...*..*..*...........*...**********...*************............******"
sample2 = "...*..*..*...........*.."
