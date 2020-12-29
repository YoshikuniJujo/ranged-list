{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.Nat where

import GHC.TypeNats

import Data.List.Length
import Data.List.Range

type RangedNatL n m = RangeL n m ()

natL :: Unfoldr' 0 n n => RangedNatL n n
natL = repeatL ()

toIntL :: Foldable (RangeL n m) => RangedNatL n m -> Int
toIntL = length

fromIntL :: Unfoldr' 0 n m => Int -> Maybe (RangedNatL n m)
fromIntL = unfoldrRangeMaybe \s -> if s > 0 then Just ((), s - 1) else Nothing

splitAtL :: ZipL n m v w => RangedNatL n m ->
	RangeL v w a -> (RangeL n m a, RangeL (v - m) (w - n) a)
splitAtL = zipWithL (flip const)

type RangedNatR n m = RangeR n m ()

natR :: Unfoldl 0 n n => RangedNatR n n
natR = repeatR ()

toIntR :: Foldable (RangeR n m) => RangedNatR n m -> Int
toIntR = length

fromIntR :: Unfoldl 0 n m => Int -> Maybe (RangedNatR n m)
fromIntR = unfoldlRangeMaybe \s -> if s > 0 then Just (s - 1, ()) else Nothing

splitAtR :: ZipR n m v w => RangeR n m a ->
	RangedNatR v w -> (RangeR (n - w) (m - v) a, RangeR v w a)
splitAtR = zipWithR const
