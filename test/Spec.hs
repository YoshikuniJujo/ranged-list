{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

import Data.Bits
import Data.Word

import Data.List.Length

main :: IO ()
main = print $ foo 123

foo :: Word32 -> LengthL 32 Bool
foo = unfoldr \w -> (w `testBit` 0, w `shiftR` 1)
