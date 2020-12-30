{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.Nat (
	-- * RangedNatL
	RangedNatL, natL, toIntL, fromIntL, splitAtL,
	-- * RangedNatR
	RangedNatR, natR, toIntR, fromIntR, splitAtR ) where

import GHC.TypeNats (type (-))
import Data.Bool (bool)
import Data.List.Length (repeatL, repeatR)
import Data.List.Range (
	RangeL, Unfoldr, unfoldrRangeMaybe, ZipL, zipWithL,
	RangeR, Unfoldl, unfoldlRangeMaybe, ZipR, zipWithR )

---------------------------------------------------------------------------

-- * RANGED NAT LEFT
-- * RANGED NAT RIGHT

---------------------------------------------------------------------------
-- RANGED NAT LEFT
---------------------------------------------------------------------------

type RangedNatL n m = RangeL n m ()

natL :: Unfoldr 0 n n => RangedNatL n n
natL = repeatL ()

toIntL :: Foldable (RangeL n m) => RangedNatL n m -> Int
toIntL = length

fromIntL :: Unfoldr 0 n m => Int -> Maybe (RangedNatL n m)
fromIntL = unfoldrRangeMaybe \s -> bool Nothing (Just ((), s - 1)) (s > 0)

splitAtL :: ZipL n m v w => RangedNatL n m ->
	RangeL v w a -> (RangeL n m a, RangeL (v - m) (w - n) a)
splitAtL = zipWithL $ flip const

---------------------------------------------------------------------------
-- RANGED NAT RIGHT
---------------------------------------------------------------------------

type RangedNatR n m = RangeR n m ()

natR :: Unfoldl 0 n n => RangedNatR n n
natR = repeatR ()

toIntR :: Foldable (RangeR n m) => RangedNatR n m -> Int
toIntR = length

fromIntR :: Unfoldl 0 n m => Int -> Maybe (RangedNatR n m)
fromIntR = unfoldlRangeMaybe \s -> bool Nothing (Just (s - 1, ())) (s > 0)

splitAtR :: ZipR n m v w => RangeR n m a ->
	RangedNatR v w -> (RangeR (n - w) (m - v) a, RangeR v w a)
splitAtR = zipWithR const
