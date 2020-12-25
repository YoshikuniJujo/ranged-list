{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Range.RangeR (
	RangeR(..), PushR, (.:++), AddR, (+++),
	LoosenRMin, loosenRMin, LoosenRMax, loosenRMax, loosenR,
	Unfoldl, unfoldlWithBaseRangeMWithS,
	ZipR, zipWithR ) where

import Control.Arrow (first, second, (***))
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

class LoosenRMin n m n' where loosenRMin :: RangeR n m a -> RangeR n' m a

instance LoosenRMin 0 m 0 where
	loosenRMin NilR = NilR
	loosenRMin xa@(_ :++ _) = xa
	loosenRMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMin (n - 1) (m - 1) 0 => LoosenRMin n m 0 where
	loosenRMin (xs :+ x) = loosenRMin xs :++ x
	loosenRMin _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMin (n - 1) (m - 1) (n' - 1) => LoosenRMin n m n' where
	loosenRMin (xs :+ x) = loosenRMin xs :+ x
	loosenRMin _ = error "never occur"

class LoosenRMax n m m' where loosenRMax :: RangeR n m a -> RangeR n m' a

instance LoosenRMax 0 0 m where
	loosenRMax NilR = NilR
	loosenRMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMax 0 (m - 1) (m' - 1) => LoosenRMax 0 m m' where
	loosenRMax NilR = NilR
	loosenRMax (xs :++ x) = loosenRMax xs :++ x
	loosenRMax _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	LoosenRMax (n - 1) (m - 1) (m' - 1) => LoosenRMax n m m' where
	loosenRMax (xs :+ x) = loosenRMax xs :+ x
	loosenRMax _ = error "never occur"

loosenR :: (LoosenRMin n m n', LoosenRMax n' m m') =>
	RangeR n m a -> RangeR n' m' a
loosenR = loosenRMax . loosenRMin

infixr 5 +++

class AddR n m v w where
	(+++) :: RangeR n m a -> RangeR v w a -> RangeR (n + v) (m + w) a

instance AddR v w 0 0 where xs +++ NilR = xs; _ +++ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenRMax n m (m + w), PushR n (m + w - 1), AddR n m 0 (w - 1)) =>
	AddR n m 0 w where
	(+++) :: forall a . RangeR n m a -> RangeR 0 w a -> RangeR n (m + w) a
	xs +++ NilR = loosenRMax xs
	xs +++ ys :++ y = (xs +++ ys :: RangeR n (m + w - 1) a) .:++ y
	_ +++ _ = error "never occur"

instance {-# OVERLAPPABLE #-} AddR n m (v - 1) (w - 1) => AddR n m v w where
	xs +++ ys :+ y = (xs +++ ys) :+ y
	_ +++ _ = error "never occur"

class Unfoldl n v w where
	unfoldlWithBaseRangeMWithS :: Monad m => (s -> Bool) ->
		(s -> m (a, s)) -> s -> RangeR n w a -> m (RangeR v w a, s)

instance Unfoldl 0 0 0 where
	unfoldlWithBaseRangeMWithS _ _ s NilR = pure (NilR, s)
	unfoldlWithBaseRangeMWithS _ _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unfoldl 0 0 (w - 1) => Unfoldl 0 0 w where
	unfoldlWithBaseRangeMWithS p f s NilR
		| p s = do
			(x, s') <- f s
			((:++ x) `first`) <$> unfoldlWithBaseRangeMWithS p f s' NilR
		| otherwise = pure (NilR, s)
	unfoldlWithBaseRangeMWithS p f s (xs :++ x) = ((:++ x) `first`) <$> unfoldlWithBaseRangeMWithS p f s xs
	unfoldlWithBaseRangeMWithS _ _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(1 <= w, Unfoldl 0 (v - 1) (w - 1)) => Unfoldl 0 v w where
	unfoldlWithBaseRangeMWithS p f s NilR = do
		(x, s') <- f s
		((:+ x) `first`) <$> unfoldlWithBaseRangeMWithS p f s' NilR
	unfoldlWithBaseRangeMWithS p f s (xs :++ x) = ((:+ x) `first`) <$> unfoldlWithBaseRangeMWithS p f s xs
	unfoldlWithBaseRangeMWithS _ _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	Unfoldl (n - 1) (v - 1) (w - 1) => Unfoldl n v w where
	unfoldlWithBaseRangeMWithS p f s (xs :+ x) = ((:+ x) `first`) <$> unfoldlWithBaseRangeMWithS p f s xs
	unfoldlWithBaseRangeMWithS _ _ _ _ = error "never occur"

class ZipR n m v w where
	zipWithR :: (a -> b -> c) -> RangeR n m a -> RangeR v w b ->
		(RangeR (n - w) (m - v) a, RangeR v w c)

instance ZipR n m 0 0 where
	zipWithR _ xs NilR = (xs, NilR)
	zipWithR _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (
	LoosenRMin n m (n - w), LoosenRMax (n - w) (m - 1) m,
	ZipR (n - 1) (m - 1) 0 (w - 1) ) => ZipR n m 0 w where
	zipWithR _ xs NilR = (loosenRMin xs, NilR)
	zipWithR f (xs :+ x) (ys :++ y) = let
		z = f x y in
		loosenRMax *** (:++ z) $ zipWithR f xs ys
	zipWithR _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-}
	(v <= m, w <= n, ZipR (n - 1) (m - 1) (v - 1) (w - 1)) => ZipR n m v w where
	zipWithR f (xs :+ x) (ys :+ y) = let
		z = f x y in
		(:+ z) `second` zipWithR f xs ys
	zipWithR _ _ _ = error "never occur"
