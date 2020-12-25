{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length.LengthL where

import GHC.TypeNats
import Control.Monad.Identity
import Data.List.Range.RangeL

type LengthL n = RangeL n n

unfoldr :: Unfoldr 0 n => (s -> (a, s)) -> s -> LengthL n a
unfoldr = unfoldrWithBase NilL

unfoldrWithBase :: Unfoldr n m => RangeL n m a -> (s -> (a, s)) -> s -> LengthL m a
unfoldrWithBase xs f s = runIdentity $ unfoldrWithBaseM xs (Identity . f) s

unfoldrM :: (Monad m, Unfoldr 0 n) => (s -> m (a, s)) -> s -> m (LengthL n a)
unfoldrM = unfoldrWithBaseM NilL

unfoldrWithBaseM' :: (Monad m, Unfoldr' n w w) => RangeL n w a -> (s -> m (a, s)) -> s -> m (LengthL w a)
unfoldrWithBaseM' xs f = (fst <$>) . unfoldrWithBaseRangeM xs undefined f

class Unfoldr n w where
	unfoldrWithBaseM :: Monad m =>
		RangeL n w a -> (s -> m (a, s)) -> s -> m (LengthL w a)

instance Unfoldr 0 0 where
	unfoldrWithBaseM NilL _ _ = pure NilL
	unfoldrWithBaseM _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= w, Unfoldr 0 (w - 1)) => Unfoldr 0 w where
	unfoldrWithBaseM NilL f s = do
		(x, s') <- f s
		(x :.) <$> unfoldrWithBaseM NilL f s'
	unfoldrWithBaseM (x :.. xs) f s = (x :.) <$> unfoldrWithBaseM xs f s
	unfoldrWithBaseM _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unfoldr (n - 1) (w - 1) => Unfoldr n w where
	unfoldrWithBaseM (x :. xs) f s = (x :.) <$> unfoldrWithBaseM xs f s
	unfoldrWithBaseM _ _ _ = error "never occur"

class ListToLengthL m where
	listToLengthL :: [a] -> Either (RangeL 0 (m - 1) a) (LengthL m a, [a])

instance ListToLengthL 1 where
	listToLengthL [] = Left NilL
	listToLengthL (x : xs) = Right (x :. NilL, xs)

instance {-# OVERLAPPABLE #-} (1 <= (m - 1), ListToLengthL (m - 1)) => ListToLengthL m where
	listToLengthL [] = Left NilL
	listToLengthL (x : xs) = case listToLengthL xs of
		Left ys -> Left $ x :.. ys
		Right (ys, xs') -> Right (x :. ys, xs')
