{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range (
	-- * RANGED LIST LEFT
	module Data.List.Range.RangeL,
	-- ** Repeat and Unfoldr Min and Max
	-- *** repeat
	repeatLMin, repeatLMax,
	-- *** unfoldr
	unfoldrMin, unfoldrMax,
	-- *** unfoldrM
	unfoldrMMin, unfoldrMMax,
	-- * RANGED LIST RIGHT
	module Data.List.Range.RangeR,
	unfoldlMin, unfoldlMinM, repeatRMin,
	unfoldlMax, unfoldlMaxM, repeatRMax,
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft ) where

import GHC.TypeNats

import Data.List.Range.RangeL
import Data.List.Range.RangeR
import Data.List.Length.LengthL
import Data.List.Length.LengthR

-- RANGED LIST LEFT

unfoldrMin :: (LoosenLMax n n m, Unfoldr 0 n n) => (s -> (a, s)) -> s -> RangeL n m a
unfoldrMin f = loosenLMax . unfoldr f

unfoldrMMin :: (Monad m, LoosenLMax n n w, Unfoldr 0 n n) =>
	(s -> m (a, s)) -> s -> m (RangeL n w a)
unfoldrMMin f s = loosenLMax <$> unfoldrM f s

repeatLMin :: (LoosenLMax n n m, Unfoldr 0 n n) => a -> RangeL n m a
repeatLMin = unfoldrMin \x -> (x, x)

unfoldrMax :: (LoosenLMin m m n, Unfoldr 0 m m) => (s -> (a, s)) -> s -> RangeL n m a
unfoldrMax f = loosenLMin . unfoldr f

unfoldrMMax :: (Monad m, LoosenLMin w w n, Unfoldr 0 w w) =>
	(s -> m (a, s)) -> s -> m (RangeL n w a)
unfoldrMMax f s = loosenLMin <$> unfoldrM f s

repeatLMax :: (LoosenLMin m m n, Unfoldr 0 m m) => a -> RangeL n m a
repeatLMax = unfoldrMax \x -> (x, x)

-- RANGED LIST RIGHT

unfoldlMin :: (LoosenRMax n n m, Unfoldl 0 n n) => (s -> (s, a)) -> s -> RangeR n m a
unfoldlMin f = loosenRMax . unfoldl f

unfoldlMinM :: (Monad m, LoosenRMax n n w, Unfoldl 0 n n) =>
	(s -> m (s, a)) -> s -> m (RangeR n w a)
unfoldlMinM f s = loosenRMax <$> unfoldlM f s

repeatRMin :: (LoosenRMax n n m, Unfoldl 0 n n) => a -> RangeR n m a
repeatRMin = unfoldlMin \x -> (x, x)

unfoldlMax :: (LoosenRMin m m n, Unfoldl 0 m m) => (s -> (s, a)) -> s -> RangeR n m a
unfoldlMax f = loosenRMin . unfoldl f

unfoldlMaxM :: (Monad m, LoosenRMin w w n, Unfoldl 0 w w) =>
	(s -> m (s, a)) -> s -> m (RangeR n w a)
unfoldlMaxM f s = loosenRMin <$> unfoldlM f s

repeatRMax :: (LoosenRMin m m n, Unfoldl 0 m m) => a -> RangeR n m a
repeatRMax = unfoldlMax \x -> (x, x)

-- LEFT TO RIGHT

infixl 5 ++.+

class LeftToRight n m v w where
	(++.+) :: RangeR n m a -> RangeL v w a -> RangeR (n + v) (m + w) a

instance LeftToRight 0 m 0 0 where n ++.+ _ = n

instance {-# OVERLAPPABLE #-} LeftToRight n m 0 0 where n ++.+ _ = n

instance {-# OVERLAPPABLE #-}
	(LoosenRMax 0 m (m + w), LeftToRight 0 (m + 1) 0 (w - 1)) =>
	LeftToRight 0 m 0 w where
	(++.+) :: forall a . RangeR 0 m a -> RangeL 0 w a -> RangeR 0 (m + w) a
	n ++.+ NilL = loosenRMax n :: RangeR 0 (m + w) a
	n ++.+ x :.. v = (n :++ x :: RangeR 0 (m + 1) a) ++.+ v
	_ ++.+ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenRMax n m (m + w), PushR (n - 1) (m - 1),
	LeftToRight n (m + 1) 0 (w - 1) ) =>
	LeftToRight n m 0 w where
	(++.+) :: forall a . RangeR n m a -> RangeL 0 w a -> RangeR n (m + w) a
	n ++.+ NilL = loosenRMax n :: RangeR n (m + w) a
	n ++.+ x :.. v= (n .:++ x :: RangeR n (m + 1) a) ++.+ v
	_ ++.+ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LeftToRight (n + 1) (m + 1) (v - 1) (w - 1) =>
	LeftToRight n m v w where
	(++.+) :: forall a .
		RangeR n m a -> RangeL v w a -> RangeR (n + v) (m + w) a
	n ++.+ x :. v = (n :+ x :: RangeR (n + 1) (m + 1) a) ++.+ v
	_ ++.+ _ = error "never occur"

leftToRight ::
	forall n m a . LeftToRight 0 0 n m => RangeL n m a -> RangeR n m a
leftToRight = ((NilR :: RangeR 0 0 a) ++.+)

-- RIGHT TO LEFT

infixr 5 ++..

class RightToLeft n m v w where
	(++..) :: RangeR n m a -> RangeL v w a -> RangeL (n + v) (m + w) a

instance RightToLeft 0 0 0 w where _ ++.. v = v

instance {-# OVERLAPPABLE #-} RightToLeft 0 0 v w where _ ++.. v = v

instance {-# OVERLAPPABLE #-}
	(LoosenLMax 0 w (m + w), RightToLeft 0 (m - 1) 0 (w + 1)) =>
	RightToLeft 0 m 0 w where
	(++..) :: forall a . RangeR 0 m a -> RangeL 0 w a -> RangeL 0 (m + w) a
	NilR ++.. v = loosenLMax v :: RangeL 0 (m + w) a
	n :++ x ++.. v = n ++.. (x :.. v :: RangeL 0 (w + 1) a)
	_ ++.. _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenLMax v w (m + w), PushL (v - 1) (w - 1),
	RightToLeft 0 (m - 1) v (w + 1) ) =>
	RightToLeft 0 m v w where
	(++..) :: forall a . RangeR 0 m a -> RangeL v w a -> RangeL v (m + w) a
	NilR ++.. v = loosenLMax v :: RangeL v (m + w) a
	n :++ x ++.. v = n ++.. (x .:.. v :: RangeL v (w + 1) a)
	_ ++.. _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	RightToLeft (n - 1) (m - 1) (v + 1) (w + 1) =>
	RightToLeft n m v w where
	(++..) :: forall a .
		RangeR n m a -> RangeL v w a -> RangeL (n + v) (m + w) a
	n :+ x ++.. v = n ++.. (x :. v :: RangeL (v + 1) (w + 1) a)
	_ ++.. _ = error "never occur"

rightToLeft ::
	forall n m a . RightToLeft n m 0 0 => RangeR n m a -> RangeL n m a
rightToLeft = (++.. (NilL :: RangeL 0 0 a))
