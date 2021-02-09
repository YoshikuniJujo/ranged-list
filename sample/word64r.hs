{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Foldable
import Data.List.Length
import Data.Bits
import Data.Word

import Bit

main :: IO ()
main = putStrLn "foo"

bitsToWord :: LengthR 64 Bit -> Word64
bitsToWord = foldr' (\b w -> w `shiftL` 1 .|. bitToNum b) 0
