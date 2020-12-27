{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Range.Nat where

import Data.Foldable
import Data.List.Length
import Data.List.Range

type RangedNatL n m = RangeL n m ()

natL :: Unfoldr 0 n n => RangedNatL n n
natL = repeatL ()

toIntL :: Foldable (RangeL n m) => RangedNatL n m -> Int
toIntL = foldl' (\n _ -> n + 1) 0

type RangedNatR n m = RangeR n m ()

natR :: Unfoldl 0 n n => RangedNatR n n
natR = repeatR ()

toIntR :: Foldable (RangeR n m) => RangedNatR n m -> Int
toIntR = foldr' (const (+ 1)) 0
