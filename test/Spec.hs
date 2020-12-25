{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Bits
import Data.Word

import Data.List.Length

main :: IO ()
main = do
	print $ foo 123
	print $ bar 123

foo :: Word32 -> LengthL 32 Bool
foo = unfoldr \w -> (w `testBit` 0, w `shiftR` 1)

bar :: Word32 -> LengthR 32 Bool
bar = unfoldl \w -> (w `testBit` 0, w `shiftR` 1)
