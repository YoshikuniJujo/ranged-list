{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length.LengthL (
	LengthL, unfoldr, unfoldrWithBase, unfoldrM, unfoldrMWithBase,
	ListToLengthL, listToLengthL ) where

import GHC.TypeNats (type (-), type (<=))
import Control.Monad.State (StateL(..))
import Data.List.Range.RangeL (RangeL(..), Unfoldr, unfoldrMRangeWithBase)

---------------------------------------------------------------------------

-- * TYPE
-- * UNFOLDR
-- * LIST TO LENGTH LEFT

---------------------------------------------------------------------------
-- TYPE
---------------------------------------------------------------------------

type LengthL n = RangeL n n

---------------------------------------------------------------------------
-- UNFOLDR
---------------------------------------------------------------------------

unfoldr :: Unfoldr 0 n n => (s -> (a, s)) -> s -> LengthL n a
unfoldr = unfoldrWithBase NilL

unfoldrWithBase ::
	Unfoldr n m m => RangeL n m a -> (s -> (a, s)) -> s -> LengthL m a
unfoldrWithBase xs f = fst . runStateL (unfoldrMWithBase xs $ StateL f)

unfoldrM :: (Monad m, Unfoldr 0 n n) => m a -> m (LengthL n a)
unfoldrM = unfoldrMWithBase NilL

unfoldrMWithBase ::
	(Monad m, Unfoldr n w w) => RangeL n w a -> m a -> m (LengthL w a)
unfoldrMWithBase = (`unfoldrMRangeWithBase` undefined)

---------------------------------------------------------------------------
-- LIST TO LENGTH LEFT
---------------------------------------------------------------------------

class ListToLengthL n where
	listToLengthL :: [a] -> Either (RangeL 0 (n - 1) a) (LengthL n a, [a])

instance ListToLengthL 1 where
	listToLengthL [] = Left NilL; listToLengthL (x : xs) = Right (x :. NilL, xs)

instance {-# OVERLAPPABLE #-}
	(1 <= (n - 1), ListToLengthL (n - 1)) => ListToLengthL n where
	listToLengthL [] = Left NilL
	listToLengthL (x : xs) = case listToLengthL xs of
		Left ys -> Left $ x :.. ys
		Right (ys, xs') -> Right (x :. ys, xs')
