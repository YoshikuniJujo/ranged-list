{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length.LengthL where

import GHC.TypeNats
import Data.List.Range.RangeL

type LengthL n = RangeL n n

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
