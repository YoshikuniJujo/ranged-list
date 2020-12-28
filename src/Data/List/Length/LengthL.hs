{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length.LengthL where

import GHC.TypeNats
import Control.Monad.Identity
import Data.List.Range.RangeL

type LengthL n = RangeL n n

unfoldr :: Unfoldr 0 n n => (s -> (a, s)) -> s -> LengthL n a
unfoldr = unfoldrWithBase NilL

unfoldrWithBase :: Unfoldr n m m => RangeL n m a -> (s -> (a, s)) -> s -> LengthL m a
unfoldrWithBase xs f s = runIdentity $ unfoldrMWithBase xs (Identity . f) s

unfoldrM :: (Monad m, Unfoldr 0 n n) => (s -> m (a, s)) -> s -> m (LengthL n a)
unfoldrM = unfoldrMWithBase NilL

unfoldrMWithBase :: (Monad m, Unfoldr n w w) => RangeL n w a -> (s -> m (a, s)) -> s -> m (LengthL w a)
unfoldrMWithBase xs f = (fst <$>) . unfoldrMWithBaseRangeWithS xs undefined f

class ListToLengthL n where
	listToLengthL :: [a] -> Either (RangeL 0 (n - 1) a) (LengthL n a, [a])

instance ListToLengthL 1 where
	listToLengthL [] = Left NilL
	listToLengthL (x : xs) = Right (x :. NilL, xs)

instance {-# OVERLAPPABLE #-} (1 <= (m - 1), ListToLengthL (m - 1)) => ListToLengthL m where
	listToLengthL [] = Left NilL
	listToLengthL (x : xs) = case listToLengthL xs of
		Left ys -> Left $ x :.. ys
		Right (ys, xs') -> Right (x :. ys, xs')
