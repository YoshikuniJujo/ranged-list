{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length.LengthR (
	LengthR, unfoldl, unfoldlWithBase, unfoldlM, unfoldlMWithBase,
	ListToLengthR, listToLengthR ) where

import GHC.TypeNats (type (-), type (<=))
import Control.Arrow (first, (+++))
import Control.Monad.State (StateR(..))
import Data.List.Range.RangeR (RangeR(..), Unfoldl, unfoldlMRangeWithBase)

---------------------------------------------------------------------------

-- TYPE
-- UNFOLDL
-- LIST TO LENGTH RIGHT

---------------------------------------------------------------------------
-- TYPE
---------------------------------------------------------------------------

type LengthR n = RangeR n n

{-^

@LengthR n a@ is a list which have just n members of type @a@.
You can push and pop an element from right.

@
sampleLengthR :: LengthR 5 Char
sampleLengthR = NilR :+ \'h\' :+ \'e\' :+ \'l\' :+ \'l\' :+ \'o\'
@

-}

---------------------------------------------------------------------------
-- UNFOLDL
---------------------------------------------------------------------------

unfoldl :: Unfoldl 0 n n => (s -> (s, a)) -> s -> LengthR n a
unfoldl f s = unfoldlWithBase f s NilR

{-^

To eveluate function repeatedly to construct a list of type @LengthR n a@.
The function recieve a state and return a new state and an element value.

@
sampleUnfoldl :: LengthR 5 Integer
sampleUnfoldl = unfoldl (\\n -> (n + 1, 2 * n)) 0
@

>>> sampleUnfoldl
((((NilR :+ 8) :+ 6) :+ 4) :+ 2) :+ 0

-}

unfoldlWithBase ::
	Unfoldl n m m => (s -> (s, a)) -> s -> RangeR n m a -> LengthR m a
unfoldlWithBase f = (snd .) . flip (runStateR . unfoldlMWithBase (StateR f))

{-^

It is like @unfoldl@. But it has already prepared values.

@
sampleUnfoldlWithBase :: LengthR 5 Integer
sampleUnfoldlWithBase =
	unfoldlWithBase (\\n -> (n + 1, 2 * n)) 0 (NilR :++ 123 :+ 456)
@

>>> sampleUnfoldlWithBase
((((NilR :+ 4) :+ 2) :+ 0) :+ 123) :+ 456

-}

unfoldlM :: (Monad m, Unfoldl 0 n n) => m a -> m (LengthR n a)
unfoldlM = (`unfoldlMWithBase` NilR)

{-^

It is like unfoldl. But it use monad as an argument instead of function.

@
sampleUnfoldlM :: IO (LengthR 3 String)
sampleUnfoldlM = unfoldlM getLine
@

>>> sampleUnfoldlM
hello
world
!
((NilR :+ "!") :+ "world") :+ "hello"

-}

unfoldlMWithBase ::
	(Monad m, Unfoldl n w w) => m a -> RangeR n w a -> m (LengthR w a)
unfoldlMWithBase = unfoldlMRangeWithBase undefined

{-^

It is like unfoldlM. But it has already prepared values.

@
sampleUnfoldlMWithBase :: IO (LengthR 5 String)
sampleUnfoldlMWithBase = unfoldlMWithBase getLine (NilR :++ "foo" :+ "bar")
@

>>> sampleUnfoldlMWithBase
hello
world
!
((((NilR :+ "!") :+ "world") :+ "hello") :+ "foo") :+ "bar"

-}

---------------------------------------------------------------------------
-- LIST TO LENGTH RIGHT
---------------------------------------------------------------------------

class ListToLengthR n where
	listToLengthR :: [a] -> Either (RangeR 0 (n - 1) a) (LengthR n a, [a])

instance ListToLengthR 1 where
	listToLengthR = \case [] -> Left NilR; x : xs -> Right (NilR :+ x, xs)

instance {-# OVERLAPPABLE #-}
	(1 <= n, 1 <= (n - 1), ListToLengthR (n - 1)) => ListToLengthR n where
	listToLengthR = \case
		[] -> Left NilR
		x : xs -> (:++ x) +++ ((:+ x) `first`) $ listToLengthR xs
