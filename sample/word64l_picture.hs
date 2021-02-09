{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Foldable
import Data.List.Length
import Data.Bits
import Data.Word

import qualified Data.List as L

data Bit = O | I deriving Show

bitToNum :: Num n => Bit -> n
bitToNum = \case O -> 0; I -> 1

boolToBit :: Bool -> Bit
boolToBit = \case False -> O; True -> I

main = putStrLn "foo"

wordToBits :: Word64 -> LengthL 64 Bit
wordToBits w = unfoldr (\x -> (boolToBit $ x `testBit` 63, x `shiftL` 1)) w

bitsToWord :: LengthL 64 Bit -> Word64
bitsToWord = foldl' (\w b -> w `shiftL` 1 .|. bitToNum b) 0

bitListToBitsList :: [Bit] -> [LengthL 64 Bit]
bitListToBitsList = chunksL' O

sample :: [String]
sample = [
	"...........................................",
	"...........................................",
	"...........................................",
	".....********...........*******............",
	"...........................................",
	"........***..............***...............",
	"......*...***..........**....*.............",
	"......*...***..........**....*.............",
	"........***..............***...............",
	"...........................................",
	"...........................................",
	"...........***************.................",
	"...........*.............*.................",
	"............*...........*..................",
	".............***********...................",
	"...........................................",
	"..........................................." ]

pack s = (bitsToWord <$>) . bitListToBitsList $ boolToBit . (== '*') <$> concat s

unpack s = unlines . (toList <$>) . fst $ chunksL @43 $ ((\case O -> '.'; I -> '*') <$>) . concat $ toList . wordToBits <$> s
