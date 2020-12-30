{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range.RangeL (
	-- ** Type
	RangeL(..),
	-- ** PushL
	PushL, (.:..),
	-- ** AddL
	AddL, (++.),
	-- ** LoosenLMin and LoosenLMax
	-- *** loosenL
	loosenL,
	-- *** loosenLMin
	LoosenLMin, loosenLMin,
	-- *** loosenLMax
	LoosenLMax, loosenLMax,
	-- ** Unfoldr
	-- *** class
	Unfoldr,
	-- *** unfoldrRange
	-- **** without monad
	unfoldrRange,
	unfoldrRangeWithBase,
	unfoldrRangeWithBaseWithS,
	-- **** with monad
	unfoldrMRange,
	unfoldrMRangeWithBase,
	-- *** unfoldrRangeMaybe
	-- **** without monad
	unfoldrRangeMaybe,
	unfoldrRangeMaybeWithBase,
	-- **** with monad
	unfoldrMRangeMaybe,
	unfoldrMRangeMaybeWithBase,
--	unfoldrMWithBaseRangeMaybe,
	-- ** ZipL
	ZipL, zipL, zipWithL, zipWithML
	) where

import GHC.TypeNats (Nat, type (+), type (-), type (<=))
import Control.Arrow (first, second, (***))
import Control.Monad.Identity
import Control.Monad.State
import Data.Bool
import Data.Maybe

infixr 6 :., :..

data RangeL :: Nat -> Nat -> * -> * where
	NilL :: RangeL 0 m a
	(:..) :: 1 <= m => a -> RangeL 0 (m - 1) a -> RangeL 0 m a
	(:.) :: a -> RangeL (n - 1) (m - 1) a -> RangeL n m a

deriving instance Show a => Show (RangeL n m a)

instance Functor (RangeL 0 0) where
	_ `fmap` NilL = NilL
	_ `fmap` _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Functor (RangeL 0 (m - 1)) => Functor (RangeL 0 m) where
	_ `fmap` NilL = NilL
	f `fmap` (x :.. xs) = f x :.. (f <$> xs)
	_ `fmap` _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Functor (RangeL (n - 1) (m - 1)) => Functor (RangeL n m) where
	f `fmap` (x :. xs) = f x :. (f <$> xs)
	_ `fmap` _ = error "never occur"

instance Foldable (RangeL 0 0) where
	foldr _ z NilL = z
	foldr _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (RangeL 0 (m - 1)) => Foldable (RangeL 0 m) where
	foldr _ z NilL = z
	foldr (-<) z (x :.. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (RangeL (n - 1) (m - 1)) => Foldable (RangeL n m) where
	foldr (-<) z (x :. xs) = x -< foldr (-<) z xs
	foldr _ _ _ = error "never occur"

infixr 5 .:..

class PushL n m where (.:..) :: a -> RangeL n m a -> RangeL n (m + 1) a

instance PushL 0 m where (.:..) = (:..)

instance {-# OVERLAPPABLE #-} PushL (n - 1) (m - 1) => PushL n m where
	x .:.. (y :. ys) = x :. (y .:.. ys)
	_ .:.. _ = error "never occur"

class LoosenLMin n m v where loosenLMin :: RangeL n m a -> RangeL v m a

instance LoosenLMin 0 m 0 where
	loosenLMin NilL = NilL
	loosenLMin xa@(_ :.. _) = xa
	loosenLMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenLMin (n - 1) (m - 1) 0 => LoosenLMin n m 0 where
	loosenLMin (x :. xs) = x :.. loosenLMin xs
	loosenLMin _ = error "never occur"
	
instance {-# OVERLAPPABLE #-}
	LoosenLMin (n - 1) (m - 1) (v - 1) => LoosenLMin n m v where
	loosenLMin (x :. xs) = x :. loosenLMin xs
	loosenLMin _ = error "never occur"

class LoosenLMax n m w where loosenLMax :: RangeL n m a -> RangeL n w a

instance LoosenLMax 0 0 w where
	loosenLMax NilL = NilL
	loosenLMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenLMax 0 (m - 1) (w - 1) => LoosenLMax 0 m w where
	loosenLMax NilL = NilL
	loosenLMax (x :.. xs) = x :.. loosenLMax xs
	loosenLMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenLMax (n - 1) (m - 1) (w - 1) => LoosenLMax n m w where
	loosenLMax (x :. xs) = x :. loosenLMax xs
	loosenLMax _ = error "never occur"

loosenL :: (LoosenLMin n m v, LoosenLMax v m w) => RangeL n m a -> RangeL v w a
loosenL = loosenLMax . loosenLMin

infixr 5 ++.

class AddL n m v w where
	(++.) :: RangeL n m a -> RangeL v w a -> RangeL (n + v) (m + w) a

instance AddL 0 0 v w where NilL ++. ys = ys; _ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(PushL v (m + w - 1), AddL 0 (m - 1) v w, LoosenLMax v w (m + w)) =>
	AddL 0 m v w where
	(++.) :: forall a .
		RangeL 0 m a -> RangeL v w a -> RangeL v (m + w) a
	NilL ++. ys = loosenLMax ys
	x :.. xs ++. ys = x .:.. (xs ++. ys :: RangeL v (m + w - 1) a)
	_ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-} AddL (n - 1) (m - 1) v w => AddL n m v w where
	x :. xs ++. ys = x :. (xs ++. ys)
	_ ++. _ = error "never occur"

unfoldrRange :: Unfoldr 0 v w => (s -> Bool) -> (s -> (a, s)) -> s -> RangeL v w a
unfoldrRange = unfoldrRangeWithBase NilL

unfoldrMRange :: (Unfoldr 0 v w, Monad m) => m Bool -> m a -> m (RangeL v w a)
unfoldrMRange = unfoldrMRangeWithBase NilL

unfoldrRangeWithBase :: Unfoldr n v w => RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> RangeL v w a
unfoldrRangeWithBase xs p f = fst . unfoldrRangeWithBaseWithS xs p f

unfoldrRangeWithBaseWithS :: Unfoldr n v w => RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> (RangeL v w a, s)
unfoldrRangeWithBaseWithS xs p f = runStateL $ unfoldrMRangeWithBase xs (StateL \s -> (p s, s)) (StateL f)

unfoldrRangeMaybe :: Unfoldr 0 v w => (s -> Maybe (a, s)) -> s -> Maybe (RangeL v w a)
unfoldrRangeMaybe = unfoldrRangeMaybeWithBase NilL

unfoldrMRangeMaybe :: (Unfoldr 0 v w, Monad m) => m Bool -> m a -> m (Maybe (RangeL v w a))
unfoldrMRangeMaybe = unfoldrMRangeMaybeWithBase NilL

unfoldrRangeMaybeWithBase :: Unfoldr n v w =>
	RangeL n w a -> (s -> Maybe (a, s)) -> s -> Maybe (RangeL v w a)
unfoldrRangeMaybeWithBase xs f s0 =
	fst $ unfoldrWithBaseRangeMaybeGen xs (\mas -> (isJust mas, mas))
		(maybe (error "never occur") (f `second`)) (f s0)

unfoldrWithBaseRangeMaybeGen :: Unfoldr n v w =>
	RangeL n w a -> (Maybe (a, s) -> (Bool, Maybe (a, s))) -> (Maybe (a, s) -> (a, Maybe (a, s))) -> Maybe (a, s) ->
	(Maybe (RangeL v w a), Maybe (a, s))
unfoldrWithBaseRangeMaybeGen xs p f =
	runStateL $ unfoldrMRangeMaybeWithBase xs (StateL p) (StateL f)

class Unfoldr n v w where
	unfoldrMRangeWithBase ::
		Monad m => RangeL n w a -> m Bool -> m a -> m (RangeL v w a)
	unfoldrMRangeMaybeWithBase ::
		Monad m => RangeL n w a -> m Bool -> m a -> m (Maybe (RangeL v w a))

instance Unfoldr 0 0 0 where
	unfoldrMRangeWithBase NilL _ _ = pure NilL
	unfoldrMRangeWithBase _ _ _ = error "never occur"

	unfoldrMRangeMaybeWithBase NilL p _ = bool (Just NilL) Nothing <$> p
	unfoldrMRangeMaybeWithBase _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unfoldr 0 0 (w - 1) => Unfoldr 0 0 w where
	unfoldrMRangeWithBase NilL p f =
		(p >>=) . bool (pure NilL) $ f >>= \x ->
			(x :..) <$> unfoldrMRangeWithBase NilL p f
	unfoldrMRangeWithBase (x :.. xs) p f =
		(x :..) <$> unfoldrMRangeWithBase xs p f
	unfoldrMRangeWithBase _ _ _ = error "never occur"

	unfoldrMRangeMaybeWithBase NilL p f =
		(p >>=) . bool (pure $ Just NilL) $ f >>= \x ->
			((x :..) <$>) <$> unfoldrMRangeMaybeWithBase NilL p f
	unfoldrMRangeMaybeWithBase (x :.. xs) p f =
		((x :..) <$>) <$> unfoldrMRangeMaybeWithBase xs p f
	unfoldrMRangeMaybeWithBase _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= w, Unfoldr 0 (v - 1) (w - 1)) => Unfoldr 0 v w where
	unfoldrMRangeWithBase NilL p f =
		f >>= \x -> (x :.) <$> unfoldrMRangeWithBase NilL p f
	unfoldrMRangeWithBase (x :.. xs) p f =
		(x :.) <$> unfoldrMRangeWithBase xs p f
	unfoldrMRangeWithBase _ _ _ = error "never occur"

	unfoldrMRangeMaybeWithBase NilL p f =
		(p >>=) . bool (pure Nothing) $ f >>= \x ->
			((x :.) <$>) <$> unfoldrMRangeMaybeWithBase NilL p f
	unfoldrMRangeMaybeWithBase (x :.. xs) p f =
		((x :.) <$>) <$> unfoldrMRangeMaybeWithBase xs p f
	unfoldrMRangeMaybeWithBase _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Unfoldr (n - 1) (v - 1) (w - 1) => Unfoldr n v w where
	unfoldrMRangeWithBase (x :. xs) p f =
		(x :.) <$> unfoldrMRangeWithBase xs p f
	unfoldrMRangeWithBase _ _ _ = error "never occur"

	unfoldrMRangeMaybeWithBase (x :. xs) p f =
		((x :.) <$>) <$> unfoldrMRangeMaybeWithBase xs p f
	unfoldrMRangeMaybeWithBase _ _ _ = error "never occur"

zipL :: ZipL n m v w => RangeL n m a -> RangeL v w b ->
	(RangeL n m (a, b), RangeL (v - m) (w - n) b)
zipL = zipWithL (,)

zipWithL :: ZipL n m v w => (a -> b -> c) -> RangeL n m a -> RangeL v w b ->
	(RangeL n m c, RangeL (v - m) (w - n) b)
zipWithL op xs ys = runIdentity $ zipWithML (\x y -> Identity $ x `op` y) xs ys

class ZipL n m v w where
	zipWithML :: Monad q =>
		(a -> b -> q c) -> RangeL n m a -> RangeL v w b ->
		q (RangeL n m c, RangeL (v - m) (w - n) b)

instance ZipL 0 0 v w where
	zipWithML _ NilL ys = pure (NilL, ys)
	zipWithML _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenLMin v w (v - m), LoosenLMax (v - m) (w - 1) w,
	ZipL 0 (m - 1) (v - 1) (w - 1) ) => ZipL 0 m v w where
	zipWithML _ NilL ys = pure (NilL, loosenLMin ys)
	zipWithML f (x :.. xs) (y :. ys) = do
		z <- f x y
		((z :..) *** loosenLMax) <$> zipWithML f xs ys
	zipWithML _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(n <= w, m <= v, ZipL (n - 1) (m - 1) (v - 1) (w - 1)) => ZipL n m v w where
	zipWithML f (x :. xs) (y :. ys) = do
		z <- f x y
		((z :.) `first`) <$> zipWithML f xs ys
	zipWithML _ _ _ = error "never occur"
