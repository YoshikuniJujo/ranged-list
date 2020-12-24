{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length (
	-- * LENGTHED LIST LEFT
	LengthL, RangeL(NilL, (:.)), AddL, (++.),
	Unfoldr, unfoldr, repeatL, fillL, listToLengthL, chunksL,
	-- * LENGTHED LIST RIGHT
	LengthR, RangeR(NilR, (:+)), AddR, (+++),
	unfoldl, repeatR, listToLengthR, chunksR,
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft
	) where

import GHC.TypeNats
import Control.Arrow (first)
import Data.List.Range
import Data.List.Length.LengthL
import Data.List.Length.LengthR

repeatL :: Unfoldr 0 n => a -> LengthL n a
repeatL = fillL NilL

fillL :: Unfoldr n m => RangeL n m a -> a -> LengthL m a
fillL xs0 = unfoldrWithBase xs0 \x -> (x, x)

chunksL :: ListToLengthL n => [a] -> ([LengthL n a], RangeL 0 (n - 1) a)
chunksL xs = case listToLengthL xs of
	Left ys -> ([], ys)
	Right (ys, xs') -> (ys :) `first` chunksL xs'

unfoldl :: UnfoldlMin n n => (s -> (a, s)) -> s -> LengthR n a
unfoldl = unfoldlMin

repeatR :: UnfoldlMin n n => a -> LengthR n a
repeatR = repeatRMin

chunksR :: ListToLengthR n => [a] -> ([LengthR n a], RangeR 0 (n - 1) a)
chunksR xs = case listToLengthR xs of
	Left ys -> ([], ys)
	Right (ys, xs') -> (ys :) `first` chunksR xs'
