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

@LengthL n a@ is a list which have just @n@ members of type @a@.

@
sampleLengthL :: Length 5 Char
sampleLengthL = 'h' :. 'e' :. 'l' :. 'l' :. 'o' :. NilL
@

-}

---------------------------------------------------------------------------
-- UNFOLDR
---------------------------------------------------------------------------

unfoldr :: Unfoldr 0 n n => (s -> (a, s)) -> s -> LengthL n a
unfoldr = unfoldrWithBase NilL

{-^

To evaluate function repeatedly to construct a list of type @LengthL n a@.
The function recieve a state and return an element value and a new state.

@
sampleUnfoldr :: LengthL 5 Integer
sampleUnfoldr = unfoldr (\n -> (2 * n, n + 1)) 0
@

>>> sampleUnfoldr
0 :. (2 :. (4 :. (6 :. (8 :. NilL))))

-}

unfoldrWithBase ::
	Unfoldr n m m => RangeL n m a -> (s -> (a, s)) -> s -> LengthL m a
unfoldrWithBase xs = (fst .) . runStateL . unfoldrMWithBase xs . StateL

{-^

It is like @unfoldr@. But it has already prepared values.

@
sampleUnfoldrWithBase :: LengthL 5 Integer
sampleUnfoldrWithBase =
	unfoldrWithBase (123 :. 456 :.. NilL) (\n -> (2 * n, n + 1)) 0
@

>>> sampleUnfoldrWithBase
123 :. (456 :. (0 :. (2 :. (4 :. NilL))))

-}

unfoldrM :: (Monad m, Unfoldr 0 n n) => m a -> m (LengthL n a)
unfoldrM = unfoldrMWithBase NilL

unfoldrMWithBase ::
	(Monad m, Unfoldr n w w) => RangeL n w a -> m a -> m (LengthL w a)
unfoldrMWithBase = (`unfoldrMRangeWithBase` undefined)

---------------------------------------------------------------------------
-- LIST TO LENGTH LEFT
---------------------------------------------------------------------------

class ListToLengthL n where
	listToLengthL :: [a] -> Either (RangeL 0 (n - 1) a) (LengthL n a, [a])

instance ListToLengthL 1 where
	listToLengthL = \case [] -> Left NilL; x : xs -> Right (x :. NilL, xs)

instance {-# OVERLAPPABLE #-}
	(1 <= n, 1 <= (n - 1), ListToLengthL (n - 1)) => ListToLengthL n where
	listToLengthL = \case
		[] -> Left NilL
		x : xs -> (x :..) +++ ((x :.) `first`) $ listToLengthL xs
