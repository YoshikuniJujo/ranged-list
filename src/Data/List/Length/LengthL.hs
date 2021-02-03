{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length.LengthL (
	LengthL, unfoldr, unfoldrWithBase, unfoldrM, unfoldrMWithBase,
	ListToLengthL, listToLengthL ) where

import GHC.TypeNats (type (-), type (<=))
import Control.Arrow (first, (+++))
import Control.Monad.State (StateL(..))
import Data.List.Range.RangeL (RangeL(..), Unfoldr, unfoldrMRangeWithBase)

---------------------------------------------------------------------------

-- * TYPE
-- * UNFOLDR
-- * LIST TO LENGTH LEFT

---------------------------------------------------------------------------
-- TYPE
---------------------------------------------------------------------------

type LengthL n = RangeL n n

{-^

The value of @LengthL n a@ is a list which have just @n@ members of type @a@.
You can push and pop an element from left.

>>> :set -XDataKinds
>>> sampleLengthL = 'h' :. 'e' :. 'l' :. 'l' :. 'o' :. NilL :: LengthL 5 Char

-}

---------------------------------------------------------------------------
-- UNFOLDR
---------------------------------------------------------------------------

unfoldr :: Unfoldr 0 n n => (s -> (a, s)) -> s -> LengthL n a
unfoldr = unfoldrWithBase NilL

{-^

To evaluate function repeatedly to construct a list of type @LengthL n a@.
The function recieve a state and return an element value and a new state.

>>> :set -XDataKinds
>>> unfoldr (\n -> (2 * n, n + 1)) 0 :: LengthL 5 Integer
0 :. (2 :. (4 :. (6 :. (8 :. NilL))))

-}

unfoldrWithBase ::
	Unfoldr n m m => RangeL n m a -> (s -> (a, s)) -> s -> LengthL m a
unfoldrWithBase xs = (fst .) . runStateL . unfoldrMWithBase xs . StateL

{-^

It is like @unfoldr@. But it has already prepared values.

>>> :set -XDataKinds
>>> sampleUnfoldrWithBase = unfoldrWithBase (123 :. 456 :.. NilL) (\n -> (2 * n, n + 1)) 0 :: LengthL 5 Integer
>>> sampleUnfoldrWithBase
123 :. (456 :. (0 :. (2 :. (4 :. NilL))))

-}

unfoldrM :: (Monad m, Unfoldr 0 n n) => m a -> m (LengthL n a)
unfoldrM = unfoldrMWithBase NilL

{-^

It is like unfoldr. But it use monad as an argument instead of function.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 0
>>> count = readIORef r >>= \n -> n <$ writeIORef r (n +1)
>>> sampleUnfoldrM = unfoldrM count :: IO (LengthL 3 Integer)
>>> sampleUnfoldrM
0 :. (1 :. (2 :. NilL))

-}

unfoldrMWithBase ::
	(Monad m, Unfoldr n w w) => RangeL n w a -> m a -> m (LengthL w a)
unfoldrMWithBase = (`unfoldrMRangeWithBase` undefined)

{-^

It is like unfoldrM. But it has already prepared values.

>>> :set -XDataKinds
>>> :module + Data.IORef
>>> r <- newIORef 0
>>> count = readIORef r >>= \n -> n <$ writeIORef r (n + 1)
>>> sampleUnfoldrMWithBase = unfoldrMWithBase (123 :. 456 :.. NilL) count :: IO (LengthL 5 Integer)
>>> sampleUnfoldrMWithBase
123 :. (456 :. (0 :. (1 :. (2 :. NilL))))

-}

---------------------------------------------------------------------------
-- LIST TO LENGTH LEFT
---------------------------------------------------------------------------

class ListToLengthL n where
	listToLengthL :: [a] -> Either (RangeL 0 (n - 1) a) (LengthL n a, [a])

	{-^

	@listToLengthL@: To take a lengthed list from a list.
	If an original list has not enough elements, then it return
	a left value.

	>>> :set -XTypeApplications -XDataKinds
	>>> listToLengthL @4 "Hi!"
	Left ('H' :.. ('i' :.. ('!' :.. NilL)))
	>>> listToLengthL @4 "Hello!"
	Right ('H' :. ('e' :. ('l' :. ('l' :. NilL))),"o!")

	-}

instance ListToLengthL 1 where
	listToLengthL = \case [] -> Left NilL; x : xs -> Right (x :. NilL, xs)

instance {-# OVERLAPPABLE #-}
	(1 <= n, 1 <= (n - 1), ListToLengthL (n - 1)) => ListToLengthL n where
	listToLengthL = \case
		[] -> Left NilL
		x : xs -> (x :..) +++ ((x :.) `first`) $ listToLengthL xs
