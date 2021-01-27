{-# LANGUAGE BlockArguments, LambdaCase #-}
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
	-- ** Repeat and Unfoldl Min and Max
	-- *** repeat
	repeatRMin, repeatRMax,
	-- *** unfoldl
	unfoldlMin, unfoldlMax,
	-- *** unfoldlM
	unfoldlMinM, unfoldlMaxM,
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft ) where

import GHC.TypeNats (type (+), type (-), type (<=))
import Data.List.Length.LengthL (unfoldr, unfoldrM)
import Data.List.Length.LengthR (unfoldl, unfoldlM)
import Data.List.Range.RangeL
import Data.List.Range.RangeR

---------------------------------------------------------------------------

-- * RANGED LIST LEFT
--	+ MIN
--	+ MAX
-- * RANGED LIST RIGHT
--	+ MIN
--	+ MAX
-- * LEFT TO RIGHT
--	+ CLASS
--	+ INSTANCE
--	+ FUNCTION
-- * RIGHT TO LEFT
--	+ CLASS
--	+ INSTANCE
--	+ FUNCTION

---------------------------------------------------------------------------
-- RANGED LIST LEFT
---------------------------------------------------------------------------

-- MIN

repeatLMin :: (LoosenLMax n n m, Unfoldr 0 n n) => a -> RangeL n m a
repeatLMin = unfoldrMin \x -> (x, x)

unfoldrMin ::
	(LoosenLMax n n m, Unfoldr 0 n n) => (s -> (a, s)) -> s -> RangeL n m a
unfoldrMin f = loosenLMax . unfoldr f

unfoldrMMin ::
	(Monad m, LoosenLMax n n w, Unfoldr 0 n n) => m a -> m (RangeL n w a)
unfoldrMMin f = loosenLMax <$> unfoldrM f

-- MAX

repeatLMax :: (LoosenLMin m m n, Unfoldr 0 m m) => a -> RangeL n m a
repeatLMax = unfoldrMax \x -> (x, x)

unfoldrMax ::
	(LoosenLMin m m n, Unfoldr 0 m m) => (s -> (a, s)) -> s -> RangeL n m a
unfoldrMax f = loosenLMin . unfoldr f

unfoldrMMax ::
	(Monad m, LoosenLMin w w n, Unfoldr 0 w w) => m a -> m (RangeL n w a)
unfoldrMMax f = loosenLMin <$> unfoldrM f

---------------------------------------------------------------------------
-- RANGED LIST RIGHT
---------------------------------------------------------------------------

-- MIN

repeatRMin :: (LoosenRMax n n m, Unfoldl 0 n n) => a -> RangeR n m a
repeatRMin = unfoldlMin \x -> (x, x)

unfoldlMin ::
	(LoosenRMax n n m, Unfoldl 0 n n) => (s -> (s, a)) -> s -> RangeR n m a
unfoldlMin f = loosenRMax . unfoldl f

unfoldlMinM ::
	(Monad m, LoosenRMax n n w, Unfoldl 0 n n) => m a -> m (RangeR n w a)
unfoldlMinM f = loosenRMax <$> unfoldlM f

-- MAX

repeatRMax :: (LoosenRMin m m n, Unfoldl 0 m m) => a -> RangeR n m a
repeatRMax = unfoldlMax \x -> (x, x)

unfoldlMax ::
	(LoosenRMin m m n, Unfoldl 0 m m) => (s -> (s, a)) -> s -> RangeR n m a
unfoldlMax f = loosenRMin . unfoldl f

unfoldlMaxM ::
	(Monad m, LoosenRMin w w n, Unfoldl 0 w w) => m a -> m (RangeR n w a)
unfoldlMaxM f = loosenRMin <$> unfoldlM f

---------------------------------------------------------------------------
-- LEFT TO RIGHT
---------------------------------------------------------------------------

-- CLASS

infixl 5 ++.+

class LeftToRight n m v w where
	(++.+) :: RangeR n m a -> RangeL v w a -> RangeR (n + v) (m + w) a

-- INSTANCE

instance LeftToRight n m 0 0 where n ++.+ _ = n

instance {-# OVERLAPPABLE #-} (
	PushR (n - 1) (m - 1), LoosenRMax n m (m + w),
	LeftToRight n (m + 1) 0 (w - 1) ) => LeftToRight n m 0 w where
	(++.+) n = \case NilL -> loosenRMax n; x :.. v -> n .:++ x ++.+ v

instance {-# OVERLAPPABLE #-}
	(1 <= v, LeftToRight (n + 1) (m + 1) (v - 1) (w - 1)) =>
	LeftToRight n m v w where
	(++.+) :: forall a .
		RangeR n m a -> RangeL v w a -> RangeR (n + v) (m + w) a
	n ++.+ x :. v = (n :+ x :: RangeR (n + 1) (m + 1) a) ++.+ v

-- FUNCTION

leftToRight ::
	forall v w a . LeftToRight 0 0 v w => RangeL v w a -> RangeR v w a
leftToRight = ((NilR :: RangeR 0 0 a) ++.+)

---------------------------------------------------------------------------
-- RIGHT TO LEFT
---------------------------------------------------------------------------

-- CLASS

infixr 5 ++..

class RightToLeft n m v w where
	(++..) :: RangeR n m a -> RangeL v w a -> RangeL (n + v) (m + w) a

-- INSTANCE

instance RightToLeft 0 0 v w where _ ++.. v = v

instance {-# OVERLAPPABLE #-} (
	PushL (v - 1) (w - 1), LoosenLMax v w (m + w),
	RightToLeft 0 (m - 1) v (w + 1) ) => RightToLeft 0 m v w where
	(++..) = \case NilR -> loosenLMax; n :++ x -> (n ++..) . (x .:..)

instance {-# OVERLAPPABLE #-} (
	1 <= n, RightToLeft (n - 1) (m - 1) (v + 1) (w + 1) ) =>
	RightToLeft n m v w where
	(++..) :: forall a .
		RangeR n m a -> RangeL v w a -> RangeL (n + v) (m + w) a
	n :+ x ++.. v = n ++.. (x :. v :: RangeL (v + 1) (w + 1) a)

-- FUNCTION

rightToLeft ::
	forall n m a . RightToLeft n m 0 0 => RangeR n m a -> RangeL n m a
rightToLeft = (++.. (NilL :: RangeL 0 0 a))
