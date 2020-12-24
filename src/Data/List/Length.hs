{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length (
	-- * LENGTHED LIST LEFT
	LengthL, RangeL(NilL, (:.)), AddL, (++.),
	unfoldr, repeatL, fillL, listToLengthL, chunksL,
	-- * LENGTHED LIST RIGHT
	LengthR, RangeR(NilR, (:+)), AddR, (+++),
	unfoldl, repeatR, listToLengthR, chunksR,
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft
	) where

import GHC.TypeNats
import Control.Arrow (first)
import Data.List.Range

type LengthL n = RangeL n n

unfoldr :: Unfoldr 0 n => (s -> (a, s)) -> s -> LengthL n a
unfoldr = unfoldrWithBase NilL

repeatL :: Unfoldr 0 n => a -> LengthL n a
repeatL = fillL NilL

fillL :: Unfoldr n m => RangeL n m a -> a -> LengthL m a
fillL xs0 = unfoldrWithBase xs0 \x -> (x, x)

class Unfoldr n w where
	unfoldrWithBase :: RangeL n w a -> (s -> (a, s)) -> s -> LengthL w a

instance Unfoldr 0 0 where
	unfoldrWithBase NilL _ _ = NilL
	unfoldrWithBase _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unfoldr 0 (w - 1) => Unfoldr 0 w where
	unfoldrWithBase NilL f s = let
		(x, s') = f s in
		x :. unfoldrWithBase NilL f s'
	unfoldrWithBase (x :.. xs) f s = x :. unfoldrWithBase xs f s
	unfoldrWithBase _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unfoldr (n - 1) (w - 1) => Unfoldr n w where
	unfoldrWithBase (x :. xs) f s = x :. unfoldrWithBase xs f s
	unfoldrWithBase _ _ _ = error "never occur"

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

chunksL :: ListToLengthL n => [a] -> ([LengthL n a], RangeL 0 (n - 1) a)
chunksL xs = case listToLengthL xs of
	Left ys -> ([], ys)
	Right (ys, xs') -> (ys :) `first` chunksL xs'

-- listToLengthL :: [a] -> Either (RangeL 0 (n - 1) a) (LengthL n a, [a])
-- listToLengthL [] = Left NilL

-- takeL :: [a] -> Maybe (LengthL n a, [a])
-- takeL =

type LengthR n = RangeR n n

unfoldl :: UnfoldlMin n n => (s -> (a, s)) -> s -> LengthR n a
unfoldl = unfoldlMin

repeatR :: UnfoldlMin n n => a -> LengthR n a
repeatR = repeatRMin

class ListToLengthR m where
	listToLengthR :: [a] -> Either (RangeR 0 (m - 1) a) (LengthR m a, [a])

instance ListToLengthR 1 where
	listToLengthR [] = Left NilR
	listToLengthR (x : xs) = Right (NilR :+ x, xs)

instance {-# OVERLAPPABLE #-} (1 <= (m - 1), ListToLengthR (m - 1)) => ListToLengthR m where
	listToLengthR [] = Left NilR
	listToLengthR (x : xs) = case listToLengthR xs of
		Left ys -> Left $ ys :++ x
		Right (ys, xs') -> Right (ys :+ x, xs')

chunksR :: ListToLengthR n => [a] -> ([LengthR n a], RangeR 0 (n - 1) a)
chunksR xs = case listToLengthR xs of
	Left ys -> ([], ys)
	Right (ys, xs') -> (ys :) `first` chunksR xs'
