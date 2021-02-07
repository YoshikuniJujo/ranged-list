{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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
	unfoldlRange, unfoldlRangeWithBase, unfoldlRangeWithBaseWithS,
	-- **** with monad
	unfoldlMRange, unfoldlMRangeWithBase,
	-- *** unfoldlRangeMaybe
	-- **** without monad
	unfoldlRangeMaybe, unfoldlRangeMaybeWithBase,
	-- **** with monad
	unfoldlMRangeMaybe, unfoldlMRangeMaybeWithBase,
	-- ** ZipR
	ZipR, zipR, zipWithR, zipWithMR ) where

import GHC.TypeNats (Nat, type (+), type (-), type (<=))
import Control.Arrow (first, second, (***), (&&&))
import Control.Monad.Identity (Identity(..))
import Control.Monad.State (StateR(..))
import Data.Bool (bool)
import Data.Maybe (isJust)

---------------------------------------------------------------------------

-- * TYPE
--	+ RANGE RIGHT
--	+ INSTANCE FUNCTOR
--	+ INSTANCE FOLDABLE
-- * PUSH
-- * ADD
-- * LOOSEN
--	+ LOOSEN RIGHT
--	+ LOOSEN RIGHT MIN
--	+ LOOSEN RIGHT MAX
-- * UNFOLDL
--	+ CLASS
--	+ INSTANCE
--	+ UNFOLDL RANGE
--	+ UNFOLDL RANGE MAYBE
-- * ZIP
--	+ CLASS AND INSTANCE
--	+ FUNCTION

---------------------------------------------------------------------------
-- TYPE
---------------------------------------------------------------------------

-- RANGE RIGHT

data RangeR :: Nat -> Nat -> * -> * where
	NilR :: 0 <= m => RangeR 0 m a
	(:++) :: 1 <= m => RangeR 0 (m - 1) a -> a -> RangeR 0 m a
	(:+) :: (1 <= n, 1 <= m) =>
		RangeR (n - 1) (m - 1) a -> a -> RangeR n m a

{-^

@RangeR n m a@ is a list of type @a@ values whose element number is
at minimum @n@, and at maximum @m@.
You can push and pop an element from right.

>>> :set -XDataKinds
>>> sampleRangeR = NilR :++ 'h' :++ 'e' :+ 'l' :+ 'l' :+ 'o' :: RangeR 3 8 Char

-}

infixl 6 :+, :++

deriving instance Show a => Show (RangeR n m a)

-- INSTANCE FUNCTOR

instance Functor (RangeR 0 0) where _ `fmap` NilR = NilR

instance {-# OVERLAPPABLE #-}
	Functor (RangeR 0 (m - 1)) => Functor (RangeR 0 m) where
	fmap f = \case NilR -> NilR; xs :++ x -> (f <$> xs) :++ f x

instance {-# OVERLAPPABLE #-}
	(1 <= n, Functor (RangeR (n - 1) (m - 1))) => Functor (RangeR n m) where
	f `fmap` (xs :+ x) = (f <$> xs) :+ f x

-- INSTANCE FOLDABLE

instance Foldable (RangeR 0 0) where foldr _ z NilR = z

instance {-# OVERLAPPABLE #-}
	Foldable (RangeR 0 (m - 1)) => Foldable (RangeR 0 m) where
	foldr (-<) z = \case NilR -> z; xs :++ x -> foldr (-<) (x -< z) xs

instance {-# OVERLAPPABLE #-} (1 <= n, Foldable (RangeR (n - 1) (m - 1))) =>
	Foldable (RangeR n m) where
	foldr (-<) z (xs :+ x) = foldr (-<) (x -< z) xs

---------------------------------------------------------------------------
-- PUSH
---------------------------------------------------------------------------

infixl 5 .:++

class PushR n m where
	(.:++) :: RangeR n m a -> a -> RangeR n (m + 1) a

	{-^

	To push an optional element.

	>>> :set -XDataKinds
	>>> samplePushR = NilR :++ 'h' :+ 'e' :+ 'l' :+ 'l' :: RangeR 3 7 Char
	>>> samplePushR .:++ 'o'
	((((NilR :++ 'h') :++ 'e') :+ 'l') :+ 'l') :+ 'o'
	>>> :type samplePushR .:++ 'o'
	samplePushR .:++ 'o' :: RangeR 3 8 Char

	-}

instance PushR 0 m where
	(.:++) = \case NilR -> (NilR :++); xs@(_ :++ _) -> (xs :++)

instance {-# OVERLAPPABLE #-} (1 <= n, PushR (n - 1) (m - 1)) => PushR n m where
	xs :+ x .:++ y = (xs .:++ x) :+ y

---------------------------------------------------------------------------
-- ADD
---------------------------------------------------------------------------

infixl 5 +++

class AddR n m v w where
	(+++) :: RangeR n m a -> RangeR v w a -> RangeR (n + v) (m + w) a

	{-^

	To concatenate two lists whose types are @RangeR n m a@ and @RangeR v w a@.

	>>> :set -XDataKinds
	>>> sampleRangeR1 = NilR :++ 'f' :+ 'o' :+ 'o' :: RangeR 2 5 Char
	>>> sampleRangeR2 = NilR :++ 'b' :++ 'a' :+ 'r' :: RangeR 1 6 Char
	>>> sampleRangeR1 +++ sampleRangeR2
	(((((NilR :++ 'f') :++ 'o') :++ 'o') :+ 'b') :+ 'a') :+ 'r'
	>>> :type sampleRangeR1 +++ sampleRangeR2
	sampleRangeR1 +++ sampleRangeR2 :: RangeR 3 11 Char

	-}

instance AddR n m 0 0 where xs +++ NilR = xs

instance {-# OVERLAPPABLE #-}
	(PushR n (m + w - 1), AddR n m 0 (w - 1), LoosenRMax n m (m + w)) =>
	AddR n m 0 w where
	(+++) :: forall a . RangeR n m a -> RangeR 0 w a -> RangeR n (m + w) a
	(+++) xs = \case
		NilR -> loosenRMax xs
		ys :++ y -> (xs +++ ys :: RangeR n (m + w - 1) a) .:++ y

instance {-# OVERLAPPABLE #-}
	(1 <= v, AddR n m (v - 1) (w - 1)) => AddR n m v w where
	xs +++ ys :+ y = (xs +++ ys) :+ y

---------------------------------------------------------------------------
-- LOOSEN
---------------------------------------------------------------------------

-- LOOSEN RIGHT

loosenR :: (LoosenRMin n m v, LoosenRMax v m w) => RangeR n m a -> RangeR v w a
loosenR = loosenRMax . loosenRMin

{-^

To loosen a range of element number.

>>> :set -XDataKinds
>>> sampleLoosenR = NilR :++ 'h' :+ 'e' :+ 'l' :+ 'l' :+ 'o' :: RangeR 4 6 Char
>>> loosenR sampleLoosenR :: RangeR 2 8 Char
((((NilR :++ 'h') :++ 'e') :++ 'l') :+ 'l') :+ 'o'

-}

-- LOOSEN RIGHT MIN

class LoosenRMin n m v where
	loosenRMin :: RangeR n m a -> RangeR v m a

	{-^

	To loosen a lower bound of element number.

	>>> :set -XDataKinds -fno-warn-tabs
	>>> :{
		sampleLoosenRMin :: RangeR 4 6 Char
		sampleLoosenRMin = NilR :++ 'h' :+ 'e' :+ 'l' :+ 'l' :+ 'o'
	:}

	>>> loosenRMin sampleLoosenRMin :: RangeR 2 6 Char
	((((NilR :++ 'h') :++ 'e') :++ 'l') :+ 'l') :+ 'o'

	-}

instance LoosenRMin 0 m 0 where
	loosenRMin = \case NilR -> NilR; xs@(_ :++ _) -> xs

instance {-# OVERLAPPABLE #-}
	(1 <= n, LoosenRMin (n - 1) (m - 1) 0) => LoosenRMin n m 0 where
	loosenRMin (xs :+ x) = loosenRMin xs :++ x

instance {-# OVERLAPPABLE #-}
	(1 <= n, LoosenRMin (n - 1) (m - 1) (v - 1)) => LoosenRMin n m v where
	loosenRMin (xs :+ x) = loosenRMin xs :+ x

-- LOOSEN RIGHT MAX

class LoosenRMax n m w where
	loosenRMax :: RangeR n m a -> RangeR n w a

	{-^

	To loosen an upper bound of element number.

	>>> :set -XDataKinds -fno-warn-tabs
	>>> :{
		sampleLoosenRMax :: RangeR 4 6 Char
		sampleLoosenRMax = NilR :++ 'h' :+ 'e' :+ 'l' :+ 'l' :+ 'o'
	:}

	>>> loosenRMax sampleLoosenRMax :: RangeR 4 8 Char
	((((NilR :++ 'h') :+ 'e') :+ 'l') :+ 'l') :+ 'o'

	-}

instance LoosenRMax 0 0 m where loosenRMax NilR = NilR

instance {-# OVERLAPPABLE #-}
	LoosenRMax 0 (m - 1) (w - 1) => LoosenRMax 0 m w where
	loosenRMax = \case NilR -> NilR; xs :++ x -> loosenRMax xs :++ x

instance {-# OVERLAPPABLE #-}
	(1 <= n, LoosenRMax (n - 1) (m - 1) (w - 1)) => LoosenRMax n m w where
	loosenRMax (xs :+ x) = loosenRMax xs :+ x

---------------------------------------------------------------------------
-- UNFOLDL
---------------------------------------------------------------------------

-- CLASS

class Unfoldl n v w where
	unfoldlMRangeWithBase :: Monad m =>
		m Bool -> m a -> RangeR n w a -> m (RangeR v w a)

	{-^

	It is like @unfoldlMRange@. But it has already prepared values.

	>>> :set -XDataKinds
	>>> :module + Data.IORef
	>>> r <- newIORef 1
	>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
	>>> xs = NilR :++ 123 :+ 456 :: RangeR 1 5 Integer
	>>> unfoldlMRangeWithBase ((< 3) <$> readIORef r) count xs :: IO (RangeR 3 5 Integer)
	(((NilR :++ 6) :+ 3) :+ 123) :+ 456

	-}

	unfoldlMRangeMaybeWithBase :: Monad m =>
		m Bool -> m a -> RangeR n w a -> m (Maybe (RangeR v w a))

	{-^

	It is like @unfoldrMRangeMaybe@. But it has already prepared values.

	>>> :set -XDataKinds
	>>> :module + Data.IORef
	>>> r <- newIORef 1
	>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
	>>> xs = NilR :++ 123 :+ 456 :: RangeR 1 5 Integer
	>>> unfoldlMRangeMaybeWithBase ((< 3) <$> readIORef r) count xs :: IO (Maybe (RangeR 3 5 Integer))
	Just ((((NilR :++ 6) :+ 3) :+ 123) :+ 456)

	-}

-- INSTANCE

instance Unfoldl 0 0 0 where
	unfoldlMRangeWithBase _ _ NilR = pure NilR
	unfoldlMRangeMaybeWithBase p _ NilR = bool (Just NilR) Nothing <$> p

instance {-# OVERLAPPABLE #-} Unfoldl 0 0 (w - 1) => Unfoldl 0 0 w where
	unfoldlMRangeWithBase p f = \case
		NilR -> (p >>=) . bool (pure NilR) $ f >>= \x ->
			(:++ x) <$> unfoldlMRangeWithBase p f NilR
		xs :++ x -> (:++ x) <$> unfoldlMRangeWithBase p f xs

	unfoldlMRangeMaybeWithBase p f = \case
		NilR -> (p >>=) . bool (pure $ Just NilR) $ f >>= \x ->
			((:++ x) <$>) <$> unfoldlMRangeMaybeWithBase p f NilR
		xs :++ x -> ((:++ x) <$>) <$> unfoldlMRangeMaybeWithBase p f xs

instance {-# OVERLAPPABLE #-} Unfoldl 0 (v - 1) (w - 1) => Unfoldl 0 v w where
	unfoldlMRangeWithBase p f = \case
		NilR -> f >>= \x -> (:+ x) <$> unfoldlMRangeWithBase p f NilR
		xs :++ x -> (:+ x) <$> unfoldlMRangeWithBase p f xs

	unfoldlMRangeMaybeWithBase p f = \case
		NilR -> (p >>=) . bool (pure Nothing) $ f >>= \x ->
			((:+ x) <$>) <$> unfoldlMRangeMaybeWithBase p f NilR
		xs :++ x -> ((:+ x) <$>) <$> unfoldlMRangeMaybeWithBase p f xs

instance {-# OVERLAPPABLE #-}
	(1 <= n, Unfoldl (n - 1) (v - 1) (w - 1)) => Unfoldl n v w where
	unfoldlMRangeWithBase p f (xs :+ x) =
		(:+ x) <$> unfoldlMRangeWithBase p f xs

	unfoldlMRangeMaybeWithBase p f (xs :+ x) =
		((:+ x) <$>) <$> unfoldlMRangeMaybeWithBase p f xs

-- UNFOLDL RANGE

unfoldlRange :: Unfoldl 0 v w =>
	(s -> Bool) -> (s -> (s, a)) -> s -> RangeR v w a
unfoldlRange p f s = unfoldlRangeWithBase p f s NilR

{-^

To eveluate a function to construct a list.
The function recieve a state and return an element and a new state.
The first argument is a predication which is evaluated when an element number is
greater than a minimum and not greater than a maximum.

>>> :set -XDataKinds
>>> unfoldlRange (< 2) (\n -> (n + 1, n * 3)) 1 :: RangeR 3 5 Int
((NilR :+ 9) :+ 6) :+ 3

>>> unfoldlRange (< 5) (\n -> (n + 1, n * 3)) 1 :: RangeR 3 5 Int
(((NilR :++ 12) :+ 9) :+ 6) :+ 3

>>> unfoldlRange (< 10) (\n -> (n + 1, n * 3)) 1 :: RangeR 3 5 Int
((((NilR :++ 15) :++ 12) :+ 9) :+ 6) :+ 3

-}

unfoldlRangeWithBase :: Unfoldl n v w =>
	(s -> Bool) -> (s -> (s, a)) -> s -> RangeR n w a -> RangeR v w a
unfoldlRangeWithBase p f = (snd .) . unfoldlRangeWithBaseWithS p f

{-^

It is like @unfoldlRange@. But it has already prepared values.

>>> :set -XDataKinds
>>> xs = NilR :++ 123 :+ 456 :: RangeR 1 5 Integer
>>> unfoldlRangeWithBase (< 3) (\n -> (n + 1, n * 3)) 1 xs :: RangeR 3 5 Integer
(((NilR :++ 6) :+ 3) :+ 123) :+ 456

-}

unfoldlRangeWithBaseWithS :: Unfoldl n v w =>
	(s -> Bool) -> (s -> (s, a)) -> s -> RangeR n w a -> (s, RangeR v w a)
unfoldlRangeWithBaseWithS p f =
	flip $ runStateR . unfoldlMRangeWithBase (StateR $ id &&& p) (StateR f)

{-^

It is like @unfoldlRangeWithBase@.
But it return not only a list but also a state value.

>>> :set -XDataKinds -fno-warn-tabs
>>> xs = NilR :++ 123 :+ 456 :: RangeR 1 5 Integer
>>> :{
	unfoldlRangeWithBaseWithS (< 3) (\n -> (n + 1, n * 3)) 1 xs
		:: (Integer, RangeR 3 5 Integer)
:}
(3,(((NilR :++ 6) :+ 3) :+ 123) :+ 456)

-}

unfoldlMRange :: (Unfoldl 0 v w, Monad m) => m Bool -> m a -> m (RangeR v w a)
unfoldlMRange p f = unfoldlMRangeWithBase p f NilR

{-^

It is like @unfoldlRange@. But it use monad instead of function.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
>>> unfoldlMRange ((< 5) <$> readIORef r) count :: IO (RangeR 3 5 Integer)
(((NilR :++ 12) :+ 9) :+ 6) :+ 3

-}

-- UNFOLDL RANGE MAYBE

unfoldlRangeMaybe ::
	Unfoldl 0 v w => (s -> Maybe (s, a)) -> s -> Maybe (RangeR v w a)
unfoldlRangeMaybe f s = unfoldlRangeMaybeWithBase f s NilR

{-^

To evaluate function to construct a list.
The function recieve state and
return a nothing value or an element and new state.
If number of created elements is less than minimum unmber of list elements or
greater than maximum number, then return Nothing.

>>> :set -XDataKinds
>>> unfoldlRangeMaybe (\n -> if n < 2 then Just (n + 1, n * 3) else Nothing) 1 :: Maybe (RangeR 3 5 Int)
Nothing

>>> unfoldlRangeMaybe (\n -> if n < 5 then Just (n + 1, n * 3) else Nothing) 1 :: Maybe (RangeR 3 5 Int)
Just ((((NilR :++ 12) :+ 9) :+ 6) :+ 3)

>>> unfoldlRangeMaybe (\n -> if n < 10 then Just (n + 1, n * 3) else Nothing) 1 :: Maybe (RangeR 3 5 Int)
Nothing

-}

unfoldlRangeMaybeWithBase :: Unfoldl n v w =>
	(s -> Maybe (s, a)) -> s -> RangeR n w a -> Maybe (RangeR v w a)
unfoldlRangeMaybeWithBase f s xs =
	snd $ unfoldlRangeMaybeWithBaseGen (id &&& isJust)
		(maybe (error "never occur") (f `first`)) xs (f s)

{-^

It is like @unfoldlRangeMaybe@. But it has already prepared values.

>>> :set -XDataKinds
>>> xs = NilR :++ 123 :+ 456 :: RangeR 1 5 Int
>>> unfoldlRangeMaybeWithBase (\n -> if n < 3 then Just (n + 1, n * 3) else Nothing) 1 xs :: Maybe (RangeR 3 5 Int)
Just ((((NilR :++ 6) :+ 3) :+ 123) :+ 456)

-}

type St s a r = Maybe (s, a) -> (Maybe (s, a), r)

unfoldlRangeMaybeWithBaseGen :: Unfoldl n v w =>
	St s a Bool -> St s a a -> RangeR n w a -> St s a (Maybe (RangeR v w a))
unfoldlRangeMaybeWithBaseGen p f =
	runStateR . unfoldlMRangeMaybeWithBase (StateR p) (StateR f)

unfoldlMRangeMaybe :: (Unfoldl 0 v w, Monad m) =>
	m Bool -> m a -> m (Maybe (RangeR v w a))
unfoldlMRangeMaybe p f = unfoldlMRangeMaybeWithBase p f NilR

{-^

It is like @unfoldlRangeMaybe@. But it use monad instead of function.
First argument monad return boolean value.
It create values while this boolean value is True.
If this boolean value is False before to create enough values or
True after to create full values, then @unfoldlMRangeMaybe@ return Nothing.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 1
>>> count = readIORef r >>= \n -> n * 3 <$ writeIORef r (n + 1)
>>> unfoldlMRangeMaybe ((< 2) <$> readIORef r) count :: IO (Maybe (RangeR 3 5 Integer))
Nothing

>>> writeIORef r 1
>>> unfoldlMRangeMaybe ((< 5) <$> readIORef r) count :: IO (Maybe (RangeR 3 5 Integer))
Just ((((NilR :++ 12) :+ 9) :+ 6) :+ 3)

>>> writeIORef r 1
>>> unfoldlMRangeMaybe ((< 10) <$> readIORef r) count :: IO (Maybe (RangeR 3 5 Integer))
Nothing

-}

---------------------------------------------------------------------------
-- ZIP
---------------------------------------------------------------------------

-- CLASS AND INSTANCE

class ZipR n m v w where
	zipWithMR :: Monad q =>
		(a -> b -> q c) -> RangeR n m a -> RangeR v w b ->
		q (RangeR (n - w) (m - v) a, RangeR v w c)

	{-^

	It is like @zipWithR@.
	But it use function which return a monad instead of a simple value.

	>>> :set -XDataKinds
	>>> ns = NilR :++ 1 :+ 2 :+ 3 :+ 4 :+ 5 :+ 6 :: RangeR 5 7 Int
	>>> cs = NilR :++ 'a' :+ 'b' :+ 'c' :: RangeR 2 4 Char
	>>> zipWithMR (\n -> putStrLn . replicate n) ns cs
	cccccc
	bbbbb
	aaaa
	(((NilR :++ 1) :++ 2) :+ 3,((NilR :++ ()) :+ ()) :+ ())

	-}

instance ZipR n m 0 0 where zipWithMR _ xs NilR = pure (xs, NilR)

instance {-# OVERLAPPABLE #-} (
	1 <= n, LoosenRMin n m (n - w), LoosenRMax (n - w) (m - 1) m,
	ZipR (n - 1) (m - 1) 0 (w - 1) ) => ZipR n m 0 w where
	zipWithMR _ xs NilR = pure (loosenRMin xs, NilR)
	zipWithMR (%) (xs :+ x) (ys :++ y) =
		x % y >>= \z -> (loosenRMax *** (:++ z)) <$> zipWithMR (%) xs ys

instance {-# OVERLAPPABLE #-} (
	1 <= n, 1 <= v, v <= m, w <= n,
	ZipR (n - 1) (m - 1) (v - 1) (w - 1) ) => ZipR n m v w where
	zipWithMR (%) (xs :+ x) (ys :+ y) =
		x % y >>= \z -> ((:+ z) `second`) <$> zipWithMR (%) xs ys

-- FUNCTION

zipR :: ZipR n m v w => RangeR n m a -> RangeR v w b ->
	(RangeR (n - w) (m - v) a, RangeR v w (a, b))
zipR = zipWithR (,)

{-^

To recieve two lists and return a tuple list and rest of the first list.
The second list must be shorter or equal than the first list.

>>> :set -XDataKinds
>>> sampleZipR1 = NilR :++ 1 :+ 2 :+ 3 :+ 4 :+ 5 :+ 6 :: RangeR 5 7 Integer
>>> sampleZipR2 = NilR :++ 3 :+ 2 :+ 1 :: RangeR 2 4 Integer
>>> zipR sampleZipR1 sampleZipR2
(((NilR :++ 1) :++ 2) :+ 3,((NilR :++ (4,3)) :+ (5,2)) :+ (6,1))
>>> :type zipR sampleZipR1 sampleZipR2
zipR sampleZipR1 sampleZipR2
  :: (RangeR 1 5 Integer, RangeR 2 4 (Integer, Integer))

-}

zipWithR :: ZipR n m v w => (a -> b -> c) -> RangeR n m a -> RangeR v w b ->
	(RangeR (n - w) (m - v) a, RangeR v w c)
zipWithR op = (runIdentity .) . zipWithMR ((Identity .) . op)

{-^

It is like @zipR@.
But it evaluate function to make values instead of put together in tuples.

>>> :set -XDataKinds
>>> sampleZipWithR1 = NilR :++ 1 :+ 2 :+ 3 :+ 4 :+ 5 :+ 6 :: RangeR 5 7 Integer
>>> sampleZipWithR2 = NilR :++ 7 :+ 6 :+ 5 :: RangeR 2 4 Integer
>>> zipWithR (+) sampleZipWithR1 sampleZipWithR2
(((NilR :++ 1) :++ 2) :+ 3,((NilR :++ 11) :+ 11) :+ 11)
>>> :type zipWithR (+) sampleZipWithR1 sampleZipWithR2
zipWithR (+) sampleZipWithR1 sampleZipWithR2
  :: (RangeR 1 5 Integer, RangeR 2 4 Integer)

-}
