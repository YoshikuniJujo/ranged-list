{-# LANGUAGE BlockArguments, TupleSections #-}
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
	-- *** class
	Unfoldr,
	-- *** without monad
	repeatL, fillL, unfoldr, unfoldrWithBase,
	-- *** with monad
	unfoldrM, unfoldrMWithBase,
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
	-- *** class
	Unfoldl,
	-- *** without monad
	repeatR, fillR, unfoldl, unfoldlWithBase,
	-- *** with monad
	unfoldlM, unfoldlMWithBase,
	-- ** ZipR
	ZipR, zipR, zipWithR, zipWithMR,
	-- ** ListToLengthR
	ListToLengthR, listToLengthR, chunksR, chunksR',
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft ) where

import GHC.TypeNats (type (-))
import Control.Arrow (first, (***))
import Data.List.Range (
	RangeL(..), AddL, (++.), LoosenLMax, loosenLMax, Unfoldr,
	ZipL, zipL, zipWithL, zipWithML,
	RangeR(..), AddR, (+++), LoosenRMax, loosenRMax, Unfoldl,
	ZipR, zipR, zipWithR, zipWithMR,
	LeftToRight, (++.+), leftToRight, RightToLeft, (++..), rightToLeft )
import Data.List.Length.LengthL (
	LengthL, unfoldr, unfoldrWithBase, unfoldrM, unfoldrMWithBase,
	ListToLengthL, listToLengthL)
import Data.List.Length.LengthR (
	LengthR, unfoldl, unfoldlWithBase, unfoldlM, unfoldlMWithBase,
	ListToLengthR, listToLengthR )

---------------------------------------------------------------------------

-- LENGTH LEFT
-- LENGTH RIGHT

---------------------------------------------------------------------------
-- LENGTH LEFT
---------------------------------------------------------------------------

repeatL :: Unfoldr 0 n n => a -> LengthL n a
repeatL = fillL NilL

{-^

To repeat a value of type @a@ to construct a list of type @LengthL n a@.

@
sampleRepeatL :: LengthL 5 Char
sampleRepeatL = repeatL \'c\'
@

>>> sampleRepeatL
'c' :. ('c' :. ('c' :. ('c' :. ('c' :. NilL))))

-}

fillL :: Unfoldr n m m => RangeL n m a -> a -> LengthL m a
fillL = (`unfoldrWithBase` \x -> (x, x))

{-^

To fill a list of type @LengthL n a@ with a value of type @a@.

@
sampleFillL :: LengthL 5 Char
sampleFillL = fillL ('a' :. 'b' :.. NilL) 'c'
@

>>> sampleFillL
'a' :. ('b' :. ('c' :. ('c' :. ('c' :. NilL))))

-}

chunksL :: ListToLengthL n => [a] -> ([LengthL n a], RangeL 0 (n - 1) a)
chunksL = either ([] ,) (uncurry first . ((:) *** chunksL)) . listToLengthL

chunksL' :: (Unfoldr 0 n n, LoosenLMax 0 (n - 1) n, ListToLengthL n) =>
	a -> [a] -> [LengthL n a]
chunksL' z xs = case chunksL xs of
	(cs, NilL) -> cs; (cs, rs) -> cs ++ [loosenLMax rs `fillL` z]

---------------------------------------------------------------------------
-- LENGTH RIGHT
---------------------------------------------------------------------------

repeatR :: Unfoldl 0 n n => a -> LengthR n a
repeatR = (`fillR` NilR)

fillR :: Unfoldl n m m => a -> RangeR n m a -> LengthR m a
fillR = unfoldlWithBase \x -> (x, x)

chunksR :: ListToLengthR n => [a] -> ([LengthR n a], RangeR 0 (n - 1) a)
chunksR = either ([] ,) (uncurry first . ((:) *** chunksR)) . listToLengthR

chunksR' :: (Unfoldl 0 n n, LoosenRMax 0 (n - 1) n, ListToLengthR n) =>
	a -> [a] -> [LengthR n a]
chunksR' z xs = case chunksR xs of
	(cs, NilR) -> cs; (cs, rs) -> cs ++ [z `fillR` loosenRMax rs]
