{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range.RangeL (
	RangeL(..), PushL, (.:..), AddL, (++.),
	LoosenLMin, loosenLMin, LoosenLMax, loosenLMax, loosenL,
	Unfoldr, unfoldrWithBaseRangeWithS, unfoldrWithBaseRangeMWithS,
	ZipL, zipL, zipWithL, zipWithML,
	unfoldrRangeMaybe, unfoldrWithBaseRangeMaybe, unfoldrWithBaseRangeMMaybe
	) where

import Control.Arrow (first, (***))
import Control.Monad.Identity
import GHC.TypeNats (Nat, type (+), type (-), type (<=))

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

class LoosenLMin n m n' where loosenLMin :: RangeL n m a -> RangeL n' m a

instance LoosenLMin 0 m 0 where
	loosenLMin NilL = NilL
	loosenLMin xa@(_ :.. _) = xa
	loosenLMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenLMin (n - 1) (m - 1) 0 => LoosenLMin n m 0 where
	loosenLMin (x :. xs) = x :.. loosenLMin xs
	loosenLMin _ = error "never occur"
	
instance {-# OVERLAPPABLE #-}
	LoosenLMin (n - 1) (m - 1) (n' - 1) => LoosenLMin n m n' where
	loosenLMin (x :. xs) = x :. loosenLMin xs
	loosenLMin _ = error "never occur"

class LoosenLMax n m m' where loosenLMax :: RangeL n m a -> RangeL n m' a

instance LoosenLMax 0 0 m where
	loosenLMax NilL = NilL
	loosenLMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenLMax 0 (m - 1) (m' - 1) => LoosenLMax 0 m m' where
	loosenLMax NilL = NilL
	loosenLMax (x :.. xs) = x :.. loosenLMax xs
	loosenLMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenLMax (n - 1) (m - 1) (m' - 1) => LoosenLMax n m m' where
	loosenLMax (x :. xs) = x :. loosenLMax xs
	loosenLMax _ = error "never occur"

loosenL :: (LoosenLMin n m n', LoosenLMax n' m m') =>
	RangeL n m a -> RangeL n' m' a
loosenL = loosenLMax . loosenLMin

infixr 5 ++.

class AddL n m v w where
	(++.) :: RangeL n m a -> RangeL v w a -> RangeL (n + v) (m + w) a

instance AddL 0 0 v w where NilL ++. ys = ys; _ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenLMax v w (m + w), PushL v (m + w - 1),
	AddL 0 (m - 1) v w ) => AddL 0 m v w where
	(++.) :: forall a .
		RangeL 0 m a -> RangeL v w a -> RangeL v (m + w) a
	NilL ++. ys = loosenLMax ys
	x :.. xs ++. ys = x .:.. (xs ++. ys :: RangeL v (m + w - 1) a)
	_ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-} AddL (n - 1) (m - 1) v w => AddL n m v w where
	x :. xs ++. ys = x :. (xs ++. ys)
	_ ++. _ = error "never occur"

unfoldrWithBaseRangeWithS :: Unfoldr n v w => RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> (RangeL v w a, s)
unfoldrWithBaseRangeWithS xs p f s = runIdentity $ unfoldrWithBaseRangeMWithS xs p (Identity . f) s

unfoldrRangeMaybe :: Unfoldr 0 v w => (s -> Maybe (a, s)) -> s -> Maybe (RangeL v w a)
unfoldrRangeMaybe = unfoldrWithBaseRangeMaybe NilL

unfoldrWithBaseRangeMaybe :: Unfoldr n v w =>
	RangeL n w a -> (s -> Maybe (a, s)) -> s -> Maybe (RangeL v w a)
unfoldrWithBaseRangeMaybe xs f s =
	runIdentity $ unfoldrWithBaseRangeMMaybe xs (Identity . f) s

class Unfoldr n v w where
	unfoldrWithBaseRangeMWithS :: Monad m => RangeL n w a ->
		(s -> Bool) -> (s -> m (a, s)) -> s -> m (RangeL v w a, s)
	unfoldrWithBaseRangeMMaybe :: Monad m => RangeL n w a ->
		(s -> m (Maybe (a, s))) -> s -> m (Maybe (RangeL v w a))

instance Unfoldr 0 0 0 where
	unfoldrWithBaseRangeMWithS NilL _ _ s = pure (NilL, s)
	unfoldrWithBaseRangeMWithS _ _ _ _ = error "never occur"

	unfoldrWithBaseRangeMMaybe NilL f s = f s >>= \case
		Just _ -> pure Nothing
		Nothing -> pure $ Just NilL
	unfoldrWithBaseRangeMMaybe _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unfoldr 0 0 (w - 1) => Unfoldr 0 0 w where
	unfoldrWithBaseRangeMWithS NilL p f s
		| p s = do
			(x, s') <- f s
			((x :..) `first`) <$> unfoldrWithBaseRangeMWithS NilL p f s'
		| otherwise = pure (NilL, s)
	unfoldrWithBaseRangeMWithS (x :.. xs) p f s = ((x :..) `first`) <$> unfoldrWithBaseRangeMWithS xs p f s
	unfoldrWithBaseRangeMWithS _ _ _ _ = error "never occur"

	unfoldrWithBaseRangeMMaybe NilL f s = f s >>= \case
		Just (x, s') -> ((x :..) <$>) <$> unfoldrWithBaseRangeMMaybe NilL f s'
		Nothing -> pure $ Just NilL
	unfoldrWithBaseRangeMMaybe (x :.. xs) f s = ((x :..) <$>) <$> unfoldrWithBaseRangeMMaybe xs f s
	unfoldrWithBaseRangeMMaybe _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= w, Unfoldr 0 (v - 1) (w - 1)) => Unfoldr 0 v w where
	unfoldrWithBaseRangeMWithS NilL p f s = do
		(x, s') <- f s
		((x :.) `first`) <$> unfoldrWithBaseRangeMWithS NilL p f s'
	unfoldrWithBaseRangeMWithS (x :.. xs) p f s = ((x :.) `first`) <$> unfoldrWithBaseRangeMWithS xs p f s
	unfoldrWithBaseRangeMWithS _ _ _ _ = error "never occur"

	unfoldrWithBaseRangeMMaybe NilL f s = f s >>= \case
		Just (x, s') -> ((x :.) <$>) <$> unfoldrWithBaseRangeMMaybe NilL f s'
		Nothing -> pure Nothing
	unfoldrWithBaseRangeMMaybe (x :.. xs) f s = ((x :.) <$>) <$> unfoldrWithBaseRangeMMaybe xs f s
	unfoldrWithBaseRangeMMaybe _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Unfoldr (n - 1) (v - 1) (w - 1) => Unfoldr n v w where
	unfoldrWithBaseRangeMWithS (x :. xs) p f s = ((x :.) `first`) <$> unfoldrWithBaseRangeMWithS xs p f s
	unfoldrWithBaseRangeMWithS _ _ _ _ = error "never occur"

	unfoldrWithBaseRangeMMaybe (x :. xs) f s = ((x :.) <$>) <$> unfoldrWithBaseRangeMMaybe xs f s
	unfoldrWithBaseRangeMMaybe _ _ _ = error "never occur"

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
