{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
	unfoldrRange, unfoldrRangeWithBase, unfoldrRangeWithBaseWithS,
	-- **** with monad
	unfoldrMRange, unfoldrMRangeWithBase,
	-- *** unfoldrRangeMaybe
	-- **** without monad
	unfoldrRangeMaybe, unfoldrRangeMaybeWithBase,
	-- **** with monad
	unfoldrMRangeMaybe, unfoldrMRangeMaybeWithBase,
	-- ** ZipL
	ZipL, zipL, zipWithL, zipWithML ) where

import GHC.TypeNats (Nat, type (+), type (-), type (<=))
import Control.Arrow (first, second, (***), (&&&))
import Control.Monad.Identity (Identity(..))
import Control.Monad.State (StateL(..))
import Data.Bool (bool)
import Data.Maybe (isJust)

---------------------------------------------------------------------------

-- * TYPE
--	+ RANGE LEFT
--	+ INSTANCE FUNCTOR
--	+ INSTANCE FOLDABLE
-- * PUSH
-- * ADD
-- * LOOSEN
-- 	+ LOOSEN LEFT
-- 	+ LOOSEN LEFT MIN
-- 	+ LOOSEN LEFT MAX
-- * UNFOLDR
-- 	+ CLASS
-- 	+ INSTANCE
-- 	+ UNFOLDR RANGE
-- 	+ UNFOLDR RANGE MAYBE
-- * ZIP
--	+ CLASS
--	+ FUNCTION

---------------------------------------------------------------------------
-- TYPE
---------------------------------------------------------------------------

-- RANGE LEFT

data RangeL :: Nat -> Nat -> * -> * where
	NilL :: 0 <= m => RangeL 0 m a
	(:..) :: 1 <= m => a -> RangeL 0 (m - 1) a -> RangeL 0 m a
	(:.) :: (1 <= n, 1 <= m) =>
		a -> RangeL (n - 1) (m - 1) a -> RangeL n m a

{-^

@RangeL n m a@ is a list of type @a@ values whose element number is
at minimum @n@, and at maximum @m@.
This list can push and pop an element from left.

@
sampleRangeL :: RangeL 3 8 Char
sampleRangeL = h :. e :. l :. l :.. o :.. NilL
@

-}

infixr 6 :., :..

deriving instance Show a => Show (RangeL n m a)

-- INSTANCE FUNCTOR

instance Functor (RangeL 0 0) where _ `fmap` NilL = NilL

instance {-# OVERLAPPABLE #-}
	Functor (RangeL 0 (m - 1)) => Functor (RangeL 0 m) where
	fmap f = \case NilL -> NilL; x :.. xs -> f x :.. (f <$> xs)

instance {-# OVERLAPPABLE #-}
	(1 <= n, Functor (RangeL (n - 1) (m - 1))) => Functor (RangeL n m) where
	f `fmap` (x :. xs) = f x :. (f <$> xs)

-- INSTANCE FOLDABLE

instance Foldable (RangeL 0 0) where foldr _ z NilL = z

instance {-# OVERLAPPABLE #-}
	Foldable (RangeL 0 (m - 1)) => Foldable (RangeL 0 m) where
	foldr (-<) z = \case NilL -> z; x :.. xs -> x -< foldr (-<) z xs

instance {-# OVERLAPPABLE #-} (1 <= n, Foldable (RangeL (n - 1) (m - 1))) =>
	Foldable (RangeL n m) where
	foldr (-<) z (x :. xs) = x -< foldr (-<) z xs

---------------------------------------------------------------------------
-- PUSH
---------------------------------------------------------------------------

infixr 5 .:..

class PushL n m where (.:..) :: a -> RangeL n m a -> RangeL n (m + 1) a

instance PushL 0 m where
	(.:..) x = \case NilL -> x :.. NilL; xs@(_ :.. _) -> x :.. xs

instance {-# OVERLAPPABLE #-} (1 <= n, PushL (n - 1) (m - 1)) => PushL n m where
	x .:.. y :. ys = x :. (y .:.. ys)

---------------------------------------------------------------------------
-- ADD
---------------------------------------------------------------------------

infixr 5 ++.

class AddL n m v w where
	(++.) :: RangeL n m a -> RangeL v w a -> RangeL (n + v) (m + w) a

{-^

Concatenation of two lists whose types are @RangeL n m a@ and @RangeL v w a@.

@
sampleRangeL1 :: RangeL 2 5 Char
sampleRangeL1 = 'f' :. 'o' :. 'o' :.. NilL

sampleRangeL2 :: RangeL 1 6 Char
sampleRangeL2 = 'b' :. 'a' :.. 'r' :.. NilL
@

>>> sampleRangeL1 ++. sampleRangeL2
'f' :. ('o' :. ('o' :. ('b' :.. ('a' :.. ('r' :.. NilL)))))
>>> :type it
it :: RangeL 3 11 Char

-}

instance AddL 0 0 v w where NilL ++. ys = ys

instance {-# OVERLAPPABLE #-}
	(PushL v (m + w - 1), AddL 0 (m - 1) v w, LoosenLMax v w (m + w)) =>
	AddL 0 m v w where
	(++.) :: forall a .  RangeL 0 m a -> RangeL v w a -> RangeL v (m + w) a
	NilL ++. ys = loosenLMax ys
	x :.. xs ++. ys = x .:.. (xs ++. ys :: RangeL v (m + w - 1) a)

instance {-# OVERLAPPABLE #-}
	(1 <= n, AddL (n - 1) (m - 1) v w) => AddL n m v w where
	x :. xs ++. ys = x :. (xs ++. ys)

---------------------------------------------------------------------------
-- LOOSEN
---------------------------------------------------------------------------

-- LOOSEN LEFT

loosenL :: (LoosenLMin n m v, LoosenLMax v m w) => RangeL n m a -> RangeL v w a
loosenL = loosenLMax . loosenLMin

-- LOOSEN LEFT MIN

class LoosenLMin n m v where loosenLMin :: RangeL n m a -> RangeL v m a

instance LoosenLMin 0 m 0 where
	loosenLMin = \case NilL -> NilL; xs@(_ :.. _) -> xs

instance {-# OVERLAPPABLE #-}
	(1 <= n, LoosenLMin (n - 1) (m - 1) 0) => LoosenLMin n m 0 where
	loosenLMin (x :. xs) = x :.. loosenLMin xs
	
instance {-# OVERLAPPABLE #-}
	(1 <= n, LoosenLMin (n - 1) (m - 1) (v - 1)) => LoosenLMin n m v where
	loosenLMin (x :. xs) = x :. loosenLMin xs

-- LOOSEN LEFT MAX

class LoosenLMax n m w where loosenLMax :: RangeL n m a -> RangeL n w a

instance LoosenLMax 0 0 w where loosenLMax NilL = NilL

instance {-# OVERLAPPABLE #-}
	LoosenLMax 0 (m - 1) (w - 1) => LoosenLMax 0 m w where
	loosenLMax = \case NilL -> NilL; (x :.. xs) -> x :.. loosenLMax xs

instance {-# OVERLAPPABLE #-}
	(1 <= n, LoosenLMax (n - 1) (m - 1) (w - 1)) => LoosenLMax n m w where
	loosenLMax (x :. xs) = x :. loosenLMax xs

---------------------------------------------------------------------------
-- UNFOLDR
---------------------------------------------------------------------------

-- CLASS

class Unfoldr n v w where
	unfoldrMRangeWithBase :: Monad m =>
		RangeL n w a -> m Bool -> m a -> m (RangeL v w a)
	unfoldrMRangeMaybeWithBase :: Monad m =>
		RangeL n w a -> m Bool -> m a -> m (Maybe (RangeL v w a))

-- INSTANCE

instance Unfoldr 0 0 0 where
	unfoldrMRangeWithBase NilL _ _ = pure NilL
	unfoldrMRangeMaybeWithBase NilL p _ = bool (Just NilL) Nothing <$> p

instance {-# OVERLAPPABLE #-} Unfoldr 0 0 (w - 1) => Unfoldr 0 0 w where
	unfoldrMRangeWithBase NilL p f =
		(p >>=) . bool (pure NilL) $ f >>= \x ->
			(x :..) <$> unfoldrMRangeWithBase NilL p f
	unfoldrMRangeWithBase (x :.. xs) p f =
		(x :..) <$> unfoldrMRangeWithBase xs p f

	unfoldrMRangeMaybeWithBase NilL p f =
		(p >>=) . bool (pure $ Just NilL) $ f >>= \x ->
			((x :..) <$>) <$> unfoldrMRangeMaybeWithBase NilL p f
	unfoldrMRangeMaybeWithBase (x :.. xs) p f =
		((x :..) <$>) <$> unfoldrMRangeMaybeWithBase xs p f

instance {-# OVERLAPPABLE #-}
	Unfoldr 0 (v - 1) (w - 1) => Unfoldr 0 v w where
	unfoldrMRangeWithBase NilL p f =
		f >>= \x -> (x :.) <$> unfoldrMRangeWithBase NilL p f
	unfoldrMRangeWithBase (x :.. xs) p f =
		(x :.) <$> unfoldrMRangeWithBase xs p f

	unfoldrMRangeMaybeWithBase NilL p f =
		(p >>=) . bool (pure Nothing) $ f >>= \x ->
			((x :.) <$>) <$> unfoldrMRangeMaybeWithBase NilL p f
	unfoldrMRangeMaybeWithBase (x :.. xs) p f =
		((x :.) <$>) <$> unfoldrMRangeMaybeWithBase xs p f

instance {-# OVERLAPPABLE #-}
	(1 <= n, Unfoldr (n - 1) (v - 1) (w - 1)) => Unfoldr n v w where
	unfoldrMRangeWithBase (x :. xs) p f =
		(x :.) <$> unfoldrMRangeWithBase xs p f

	unfoldrMRangeMaybeWithBase (x :. xs) p f =
		((x :.) <$>) <$> unfoldrMRangeMaybeWithBase xs p f

-- UNFOLDR RANGE

unfoldrRange :: Unfoldr 0 v w =>
	(s -> Bool) -> (s -> (a, s)) -> s -> RangeL v w a
unfoldrRange = unfoldrRangeWithBase NilL

unfoldrRangeWithBase :: Unfoldr n v w =>
	RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> RangeL v w a
unfoldrRangeWithBase xs p f = fst . unfoldrRangeWithBaseWithS xs p f

unfoldrRangeWithBaseWithS :: Unfoldr n v w =>
	RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> (RangeL v w a, s)
unfoldrRangeWithBaseWithS xs p f =
	runStateL $ unfoldrMRangeWithBase xs (StateL $ p &&& id) (StateL f)

unfoldrMRange :: (Unfoldr 0 v w, Monad m) => m Bool -> m a -> m (RangeL v w a)
unfoldrMRange = unfoldrMRangeWithBase NilL

-- UNFOLDR RANGE MAYBE

unfoldrRangeMaybe :: Unfoldr 0 v w =>
	(s -> Maybe (a, s)) -> s -> Maybe (RangeL v w a)
unfoldrRangeMaybe = unfoldrRangeMaybeWithBase NilL

unfoldrRangeMaybeWithBase :: Unfoldr n v w =>
	RangeL n w a -> (s -> Maybe (a, s)) -> s -> Maybe (RangeL v w a)
unfoldrRangeMaybeWithBase xs f =
	fst . unfoldrRangeMaybeWithBaseGen xs (isJust &&& id)
		(maybe (error "never occur") (f `second`)) . f

type St a s r = Maybe (a, s) -> (r, Maybe (a, s))

unfoldrRangeMaybeWithBaseGen :: Unfoldr n v w =>
	RangeL n w a -> St a s Bool -> St a s a -> St a s (Maybe (RangeL v w a))
unfoldrRangeMaybeWithBaseGen xs p f =
	runStateL $ unfoldrMRangeMaybeWithBase xs (StateL p) (StateL f)

unfoldrMRangeMaybe :: (Unfoldr 0 v w, Monad m) =>
	m Bool -> m a -> m (Maybe (RangeL v w a))
unfoldrMRangeMaybe = unfoldrMRangeMaybeWithBase NilL

---------------------------------------------------------------------------
-- ZIP
---------------------------------------------------------------------------

-- CLASS

class ZipL n m v w where
	zipWithML :: Monad q =>
		(a -> b -> q c) -> RangeL n m a -> RangeL v w b ->
		q (RangeL n m c, RangeL (v - m) (w - n) b)

{-^

@zipWithML@ is like zipWithL.
But it use function which return monad instead of a simple value.

@
sampleZipWithML1 :: RangeL 2 4 Int
sampleZipWithML1 = 1 :. 2 :. 3 :.. NilL

sampleZipWithML2 :: RangeL 5 7 Char
sampleZipWithML2 = 'a' :. 'b' :. 'c' :. 'd' :. 'e' :. 'f' :.. NilL
@

>>> zipWithML (\n -> putStrLn . replicate n) samplezipWithML1 sampleZipWithML2
a
bb
ccc
(() :. (() :. (() :.. NilL)),'d' :. ('e' :.. ('f' :.. NilL)))

-}

instance ZipL 0 0 v w where zipWithML _ NilL = pure . (NilL ,)

instance {-# OVERLAPPABLE #-} (
	1 <= v, LoosenLMin v w (v - m), LoosenLMax (v - m) (w - 1) w,
	ZipL 0 (m - 1) (v - 1) (w - 1) ) => ZipL 0 m v w where
	zipWithML _ NilL ys = pure (NilL, loosenLMin ys)
	zipWithML (%) (x :.. xs) (y :. ys) =
		x % y >>= \z -> ((z :..) *** loosenLMax) <$> zipWithML (%) xs ys

instance {-# OVERLAPPABLE #-} (
	1 <= n, 1 <= v, n <= w, m <= v,
	ZipL (n - 1) (m - 1) (v - 1) (w - 1) ) => ZipL n m v w where
	zipWithML (%) (x :. xs) (y :. ys) =
		x % y >>= \z -> ((z :.) `first`) <$> zipWithML (%) xs ys

-- FUNCTION

zipL :: ZipL n m v w => RangeL n m a -> RangeL v w b ->
	(RangeL n m (a, b), RangeL (v - m) (w - n) b)
zipL = zipWithL (,)

{-^

To recieve two lists and return a tuple list and rest of second list.
First list must be shorter or equal than second list.

@
sampleZipL1 :: RangeL 2 4 Integer
sampleZipL1 = 1 :. 2 :. 3 :.. NilL

sampleZipL2 :: RangeL 5 7 Integer
sampleZipL2 = 7 :. 6 :. 5 :. 4 :. 3 :. 2 :.. NilL
@

>>> zipL sampleZipL1 sampleZipL2
((1,7) :. ((2,6) :. ((3,5) :.. NilL)),4 :. (3 :.. (2 :.. NilL)))
>>> :type it
it :: (RangeL 2 4 (Integer, Integer), RangeL 1 5 Integer)

-}

zipWithL :: ZipL n m v w => (a -> b -> c) -> RangeL n m a -> RangeL v w b ->
	(RangeL n m c, RangeL (v - m) (w - n) b)
zipWithL op = (runIdentity .) . zipWithML ((Identity .) . op)

{-^

It is like @zipL@.
But it evaluate function to make values instead of put together in tuples.

@
sampleZipWithL1 :: RangeL 2 4 Integer
sampleZipWithL1 = 1 :. 2 :. 3 :.. NilL

sampleZipWithL2 :: RangeL 5 7 Integer
sampleZipWithL2 = 7 :. 6 :. 5 :. 4 :. 3 :. 2 :.. NilL
@

>>> zipWithL (+) sampleZipWithL1 sampleZipWithL2
(8 :. (8 :. (8 :.. NilL)),4 :. (3 :.. (2 :.. NilL)))
>>> :type it
it :: (RangeL 2 4 Integer, RangeL 1 5 Integer)

-}
