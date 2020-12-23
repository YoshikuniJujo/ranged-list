{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length (
	-- * LENGTHED LIST LEFT
	LengthL, RangeL(NilL, (:.)), AddL, (++.), unfoldr, repeatL,
	-- * LENGTHED LIST RIGHT
	LengthR, RangeR(NilR, (:+)), AddR, (+++), unfoldl, repeatR,
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft
	) where

import Data.List.Range

type LengthL n = RangeL n n

unfoldr :: UnfoldrMin n n => (s -> (a, s)) -> s -> LengthL n a
unfoldr = unfoldrMin

repeatL :: UnfoldrMin n n => a -> LengthL n a
repeatL = repeatLMin

type LengthR n = RangeR n n

unfoldl :: UnfoldlMin n n => (s -> (a, s)) -> s -> LengthR n a
unfoldl = unfoldlMin

repeatR :: UnfoldlMin n n => a -> LengthR n a
repeatR = repeatRMin
