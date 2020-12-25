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
	Unfoldr', unfoldrWithBaseRange ) where

import Control.Arrow (first)
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

class AddL n m n' m' where
	(++.) :: RangeL n m a -> RangeL n' m' a -> RangeL (n + n') (m + m') a

instance AddL 0 0 n' m' where NilL ++. ys = ys; _ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenLMax n' m' (m + m'), PushL n' (m + m' - 1),
	AddL 0 (m - 1) n' m' ) => AddL 0 m n' m' where
	(++.) :: forall a .
		RangeL 0 m a -> RangeL n' m' a -> RangeL n' (m + m') a
	NilL ++. ys = loosenLMax ys
	x :.. xs ++. ys = x .:.. (xs ++. ys :: RangeL n' (m + m' - 1) a)
	_ ++. _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	AddL (n - 1) (m - 1) n' m' => AddL n m n' m' where
	x :. xs ++. ys = x :. (xs ++. ys)
	_ ++. _ = error "never occur"

unfoldrWithBaseRange :: Unfoldr' n v w => RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> (RangeL v w a, s)
unfoldrWithBaseRange xs p f s = runIdentity $ unfoldrWithBaseRangeM xs p (Identity . f) s

class Unfoldr' n v w where
	unfoldrWithBaseRangeM :: Monad m => RangeL n w a ->
		(s -> Bool) -> (s -> m (a, s)) -> s -> m (RangeL v w a, s)

instance Unfoldr' 0 0 0 where
	unfoldrWithBaseRangeM NilL _ _ s = pure (NilL, s)
	unfoldrWithBaseRangeM _ _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Unfoldr' 0 0 (w - 1) => Unfoldr' 0 0 w where
	unfoldrWithBaseRangeM NilL p f s
		| p s = do
			(x, s') <- f s
			((x :..) `first`) <$> unfoldrWithBaseRangeM NilL p f s'
		| otherwise = pure (NilL, s)
	unfoldrWithBaseRangeM (x :.. xs) p f s = ((x :..) `first`) <$> unfoldrWithBaseRangeM xs p f s
	unfoldrWithBaseRangeM _ _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Unfoldr' 0 (v - 1) (w - 1) => Unfoldr' 0 v w where
	unfoldrWithBaseRangeM NilL p f s = do
		(x, s') <- f s
		((x :.) `first`) <$> unfoldrWithBaseRangeM NilL p f s'
	unfoldrWithBaseRangeM (x :.. xs) p f s = ((x :.) `first`) <$> unfoldrWithBaseRangeM xs p f s
	unfoldrWithBaseRangeM _ _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Unfoldr' (n - 1) (v - 1) (w - 1) => Unfoldr' n v w where
	unfoldrWithBaseRangeM (x :. xs) p f s = ((x :.) `first`) <$> unfoldrWithBaseRangeM xs p f s
	unfoldrWithBaseRangeM _ _ _ _ = error "never occur"
