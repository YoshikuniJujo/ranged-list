{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length (
	-- * LENGTHED LIST LEFT
	-- ** Type
	LengthL, RangeL(NilL, (:.)),
	-- ** AddL
	AddL, (++.),
	-- ** Unfoldr
	Unfoldr,
	repeatL, fillL, unfoldr, unfoldrM', unfoldrWithBase, unfoldrMWithBase',
	-- ** ZipL
	ZipL, zipL, zipWithL, zipWithML,
	-- ** ListToLengthL
	ListToLengthL, listToLengthL, chunksL, chunksL',
	-- * LENGTHED LIST RIGHT
	-- ** Type
	LengthR, RangeR(NilR, (:+)),
	-- ** AddR
	AddR, (+++),
	-- ** Unfoldl
	Unfoldl,
	repeatR, fillR, unfoldl, unfoldlM, unfoldlWithBase, unfoldlWithBaseM,
	-- ** ZipR
	ZipR, zipR, zipWithR, zipWithMR,
	-- ** ListToLengthR
	listToLengthR, chunksR, chunksR',
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

repeatL :: Unfoldr 0 n n => a -> LengthL n a
repeatL = fillL NilL

fillL :: Unfoldr n m m => RangeL n m a -> a -> LengthL m a
fillL xs0 = unfoldrWithBase xs0 \x -> (x, x)

chunksL :: ListToLengthL n => [a] -> ([LengthL n a], RangeL 0 (n - 1) a)
chunksL xs = case listToLengthL xs of
	Left ys -> ([], ys)
	Right (ys, xs') -> (ys :) `first` chunksL xs'

chunksL' :: (Unfoldr 0 n n, ListToLengthL n, LoosenLMax 0 (n - 1) n) => a -> [a] -> [LengthL n a]
chunksL' z xs = case chunksL xs of
	(cs, NilL) -> cs
	(cs, rs) -> cs ++ [fillL (loosenLMax rs) z]

repeatR :: Unfoldl 0 n n => a -> LengthR n a
repeatR = (`fillR` NilR)

fillR :: Unfoldl n m m => a -> RangeR n m a -> LengthR m a
fillR = unfoldlWithBase \x -> (x, x)

chunksR :: ListToLengthR n => [a] -> ([LengthR n a], RangeR 0 (n - 1) a)
chunksR xs = case listToLengthR xs of
	Left ys -> ([], ys)
	Right (ys, xs') -> (ys :) `first` chunksR xs'

chunksR' :: (Unfoldl 0 n n, ListToLengthR n, LoosenRMax 0 (n - 1) n) => a -> [a] -> [LengthR n a]
chunksR' z xs = case chunksR xs of
	(cs, NilR) -> cs
	(cs, rs) -> cs ++ [fillR z $ loosenRMax rs]
