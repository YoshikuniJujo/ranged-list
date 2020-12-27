{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.Nat where

import Data.List.Length
import Data.List.Range

type RangedNatL n m = RangeL n m ()

natL :: Unfoldr 0 n n => RangedNatL n n
natL = repeatL ()

toIntL :: Foldable (RangeL n m) => RangedNatL n m -> Int
toIntL = length

fromIntL :: Unfoldr 0 n m => Int -> Maybe (RangedNatL n m)
fromIntL = unfoldrRangeMaybe \s -> if s > 0 then Just ((), s - 1) else Nothing

type RangedNatR n m = RangeR n m ()

natR :: Unfoldl 0 n n => RangedNatR n n
natR = repeatR ()

toIntR :: Foldable (RangeR n m) => RangedNatR n m -> Int
toIntR = length

fromIntR :: Unfoldl 0 n m => Int -> Maybe (RangedNatR n m)
fromIntR = unfoldlRangeMaybe \s -> if s > 0 then Just ((), s - 1) else Nothing
