{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range.RangeR (
	-- ** Type
	RangeR(..),
	-- ** PushR
	PushR, (.:++),
	-- ** AddR
	AddR, (+++),
	-- ** LoosenRMin and LoosenRMax
	-- *** loosenR
	loosenR,
	-- *** loosenRMin
	LoosenRMin, loosenRMin,
	-- *** loosenRMax
	LoosenRMax, loosenRMax,
	-- ** Unfoldl
	-- *** class
	Unfoldl,
	-- *** unfoldlRange
	-- **** without monad
	-- **** with monad
	unfoldlMRange, unfoldlMRangeWithBase,
	-- *** unfoldlRangeMaybe
	unfoldlRangeMaybe,
	-- **** without monad
	unfoldlMRangeMaybeWithBase,
	-- **** with monad
	-- ** ZipR
	ZipR, zipR, zipWithR, zipWithMR ) where

import Control.Arrow (second, (***))
import Control.Monad.Identity
import Control.Monad.State
import Data.Bool
import Data.Maybe
import GHC.TypeLits

infixl 6 :+, :++

data RangeR :: Nat -> Nat -> * -> * where
	NilR :: RangeR 0 m a
	(:++) :: 1 <= m => RangeR 0 (m - 1) a -> a -> RangeR 0 m a
	(:+) :: RangeR (n - 1) (m - 1) a -> a -> RangeR n m a

deriving instance Show a => Show (RangeR n m a)

instance Functor (RangeR 0 0) where
	_ `fmap` NilR = NilR
	_ `fmap` _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Functor (RangeR 0 (m - 1)) => Functor (RangeR 0 m) where
	_ `fmap` NilR = NilR
	f `fmap` (xs :++ x) = (f <$> xs) :++ f x
	_ `fmap` _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Functor (RangeR (n - 1) (m - 1)) => Functor (RangeR n m) where
	f `fmap` (xs :+ x) = (f <$> xs) :+ f x
	_ `fmap` _ = error "never occur"

instance Foldable (RangeR 0 0) where
	foldr _ z NilR = z
	foldr _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (RangeR 0 (m - 1)) => Foldable (RangeR 0 m) where
	foldr _ z NilR = z
	foldr (-<) z (xs :++ x) = foldr (-<) (x -< z) xs
	foldr _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Foldable (RangeR (n - 1) (m - 1)) => Foldable (RangeR n m) where
	foldr (-<) z (xs :+ x) = foldr (-<) (x -< z) xs
	foldr _ _ _ = error "never occur"

infixl 5 .:++

class PushR n m where (.:++) :: RangeR n m a -> a -> RangeR n (m + 1) a

instance PushR 0 m where (.:++) = (:++)

instance {-# OVERLAPPABLE #-} PushR (n - 1) (m - 1) => PushR n m where
	xs :+ x .:++ y = (xs .:++ x) :+ y
	_ .:++ _ = error "never occur"

class LoosenRMin n m v where loosenRMin :: RangeR n m a -> RangeR v m a

instance LoosenRMin 0 m 0 where
	loosenRMin NilR = NilR
	loosenRMin xa@(_ :++ _) = xa
	loosenRMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMin (n - 1) (m - 1) 0 => LoosenRMin n m 0 where
	loosenRMin (xs :+ x) = loosenRMin xs :++ x
	loosenRMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMin (n - 1) (m - 1) (v - 1) => LoosenRMin n m v where
	loosenRMin (xs :+ x) = loosenRMin xs :+ x
	loosenRMin _ = error "never occur"

class LoosenRMax n m w where loosenRMax :: RangeR n m a -> RangeR n w a

instance LoosenRMax 0 0 m where
	loosenRMax NilR = NilR
	loosenRMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMax 0 (m - 1) (w - 1) => LoosenRMax 0 m w where
	loosenRMax NilR = NilR
	loosenRMax (xs :++ x) = loosenRMax xs :++ x
	loosenRMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMax (n - 1) (m - 1) (w - 1) => LoosenRMax n m w where
	loosenRMax (xs :+ x) = loosenRMax xs :+ x
	loosenRMax _ = error "never occur"

loosenR :: (LoosenRMin n m v, LoosenRMax v m w) =>
	RangeR n m a -> RangeR v w a
loosenR = loosenRMax . loosenRMin

infixr 5 +++

class AddR n m v w where
	(+++) :: RangeR n m a -> RangeR v w a -> RangeR (n + v) (m + w) a

instance AddR n m 0 0 where xs +++ NilR = xs; _ +++ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	PushR n (m + w - 1), AddR n m 0 (w - 1), LoosenRMax n m (m + w)) =>
	AddR n m 0 w where
	(+++) :: forall a . RangeR n m a -> RangeR 0 w a -> RangeR n (m + w) a
	xs +++ NilR = loosenRMax xs
	xs +++ ys :++ y = (xs +++ ys :: RangeR n (m + w - 1) a) .:++ y
	_ +++ _ = error "never occur"

instance {-# OVERLAPPABLE #-} AddR n m (v - 1) (w - 1) => AddR n m v w where
	xs +++ ys :+ y = (xs +++ ys) :+ y
	_ +++ _ = error "never occur"

unfoldlMRange :: (Unfoldl 0 v w, Monad m) => m Bool -> m a -> m (RangeR v w a)
unfoldlMRange p f = unfoldlMRangeWithBase p f NilR

unfoldlRangeMaybe :: Unfoldl 0 v w => (s -> Maybe (s, a)) -> s -> Maybe (RangeR v w a)
unfoldlRangeMaybe f s = unfoldlWithBaseRangeMaybe f s NilR

unfoldlWithBaseRangeMaybe :: Unfoldl n v w => (s -> Maybe (s, a)) -> s -> RangeR n w a -> Maybe (RangeR v w a)
unfoldlWithBaseRangeMaybe f s0 xs =
	fst $ unfoldlRangeMaybeWithBaseGen (\mas -> (isJust mas, mas))
		(maybe (error "never occur") (\(s, x) -> (x, f s))) xs (f s0)

unfoldlRangeMaybeWithBaseGen :: Unfoldl n v w =>
	(Maybe (s, a) -> (Bool, Maybe (s, a))) ->
	(Maybe (s, a) -> (a, (Maybe (s, a)))) -> RangeR n w a ->
	Maybe (s, a) -> (Maybe (RangeR v w a), Maybe (s, a))
unfoldlRangeMaybeWithBaseGen p f xs =
	runStateL $ unfoldlMRangeMaybeWithBase (StateL p) (StateL f) xs

class Unfoldl n v w where
	unfoldlMRangeWithBase :: Monad m =>
		m Bool -> m a -> RangeR n w a -> m (RangeR v w a)
	unfoldlMRangeMaybeWithBase :: Monad m =>
		m Bool -> m a -> RangeR n w a -> m (Maybe (RangeR v w a))

instance Unfoldl 0 0 0 where
	unfoldlMRangeWithBase _ _ NilR = pure NilR
	unfoldlMRangeWithBase _ _ _ = error "never occur"

	unfoldlMRangeMaybeWithBase p _ NilR = bool (Just NilR) Nothing <$> p
	unfoldlMRangeMaybeWithBase _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Unfoldl 0 0 (w - 1) => Unfoldl 0 0 w where
	unfoldlMRangeWithBase p f NilR =
		(p >>=) . bool (pure NilR) $ f >>= \x ->
			(:++ x) <$> unfoldlMRangeWithBase p f NilR
	unfoldlMRangeWithBase p f (xs :++ x) =
		(:++ x) <$> unfoldlMRangeWithBase p f xs
	unfoldlMRangeWithBase _ _ _ = error "never occur"

	unfoldlMRangeMaybeWithBase p f NilR =
		(p >>=) . bool (pure $ Just NilR) $ f >>= \x ->
			((:++ x) <$>) <$> unfoldlMRangeMaybeWithBase p f NilR
	unfoldlMRangeMaybeWithBase p f (xs :++ x) =
		((:++ x) <$>) <$> unfoldlMRangeMaybeWithBase p f xs
	unfoldlMRangeMaybeWithBase _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= w, Unfoldl 0 (v - 1) (w - 1)) => Unfoldl 0 v w where
	unfoldlMRangeWithBase p f NilR =
		f >>= \x -> (:+ x) <$> unfoldlMRangeWithBase p f NilR
	unfoldlMRangeWithBase p f (xs :++ x) =
		(:+ x) <$> unfoldlMRangeWithBase p f xs
	unfoldlMRangeWithBase _ _ _ = error "never occur"

	unfoldlMRangeMaybeWithBase p f NilR =
		(p >>=) . bool (pure Nothing) $ f >>= \x ->
			((:+ x) <$>) <$> unfoldlMRangeMaybeWithBase p f NilR
	unfoldlMRangeMaybeWithBase p f (xs :++ x) =
		((:+ x) <$>) <$> unfoldlMRangeMaybeWithBase p f xs
	unfoldlMRangeMaybeWithBase _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Unfoldl (n - 1) (v - 1) (w - 1) => Unfoldl n v w where
	unfoldlMRangeWithBase p f (xs :+ x) =
		(:+ x) <$> unfoldlMRangeWithBase p f xs
	unfoldlMRangeWithBase _ _ _ = error "never occur"

	unfoldlMRangeMaybeWithBase p f (xs :+ x) =
		((:+ x) <$>) <$> unfoldlMRangeMaybeWithBase p f xs
	unfoldlMRangeMaybeWithBase _ _ _ = error "never occur"

zipR :: ZipR n m v w => RangeR n m a -> RangeR v w b ->
	(RangeR (n - w) (m - v) a, RangeR v w (a, b))
zipR = zipWithR (,)

zipWithR :: ZipR n m v w => (a -> b -> c) -> RangeR n m a -> RangeR v w b ->
	(RangeR (n - w) (m - v) a, RangeR v w c)
zipWithR op xs ys = runIdentity $ zipWithMR (\x y -> Identity $ x `op` y) xs ys

class ZipR n m v w where
	zipWithMR :: Monad q =>
		(a -> b -> q c) -> RangeR n m a -> RangeR v w b ->
		q (RangeR (n - w) (m - v) a, RangeR v w c)

instance ZipR n m 0 0 where
	zipWithMR _ xs NilR = pure (xs, NilR)
	zipWithMR _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenRMin n m (n - w), LoosenRMax (n - w) (m - 1) m,
	ZipR (n - 1) (m - 1) 0 (w - 1) ) => ZipR n m 0 w where
	zipWithMR _ xs NilR = pure (loosenRMin xs, NilR)
	zipWithMR f (xs :+ x) (ys :++ y) = do
		z <- f x y
		(loosenRMax *** (:++ z)) <$> zipWithMR f xs ys
	zipWithMR _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(v <= m, w <= n, ZipR (n - 1) (m - 1) (v - 1) (w - 1)) => ZipR n m v w where
	zipWithMR f (xs :+ x) (ys :+ y) = do
		z <- f x y
		((:+ z) `second`) <$> zipWithMR f xs ys
	zipWithMR _ _ _ = error "never occur"
