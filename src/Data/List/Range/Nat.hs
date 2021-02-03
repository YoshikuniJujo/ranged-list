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

{-^

To make @RangedNatL@.

>>> :set -XDataKinds
>>> :module + Data.List.Range
>>> n5 = natL :: RangedNatL 5 5
>>> loosenL n5 :: RangedNatL 3 8
() :. (() :. (() :. (() :.. (() :.. NilL))))

-}

toIntL :: Foldable (RangeL n m) => RangedNatL n m -> Int
toIntL = length

{-^

To convert from @RangedNatL@ to @Int@.

>>> :set -XTypeApplications -XDataKinds
>>> :module + Data.List.Range
>>> toIntL (loosenL (natL @5) :: RangedNatL 3 8)
5

-}

fromIntL :: Unfoldr 0 n m => Int -> Maybe (RangedNatL n m)
fromIntL = unfoldrRangeMaybe \s -> bool Nothing (Just ((), s - 1)) (s > 0)

{-^

To convert from @Int@ to @RangedNatL@.

>>> :set -XDataKinds
>>> fromIntL 5 :: Maybe (RangedNatL 3 8)
Just (() :. (() :. (() :. (() :.. (() :.. NilL)))))

-}

splitAtL :: ZipL n m v w => RangedNatL n m ->
	RangeL v w a -> (RangeL n m a, RangeL (v - m) (w - n) a)
splitAtL = zipWithL $ flip const

{-^

To split list at position which is specified by number.

>>> :set -XTypeApplications -XDataKinds
>>> :module + Data.List.Range
>>> xs = 'h' :. 'e' :. 'l' :. 'l' :.. 'o' :.. NilL :: RangeL 3 8 Char
>>> splitAtL (natL @2) xs
('h' :. ('e' :. NilL),'l' :. ('l' :.. ('o' :.. NilL)))
>>> :type splitAtL (natL @2) xs
splitAtL (natL @2) xs :: (RangeL 2 2 Char, RangeL 1 6 Char)

-}

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
