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

---------------------------------------------------------------------------
-- UNFOLDL
---------------------------------------------------------------------------

unfoldl :: Unfoldl 0 n n => (s -> (s, a)) -> s -> LengthR n a
unfoldl f s = unfoldlWithBase f s NilR

unfoldlWithBase ::
	Unfoldl n m m => (s -> (s, a)) -> s -> RangeR n m a -> LengthR m a
unfoldlWithBase f = (snd .) . flip (runStateR . unfoldlMWithBase (StateR f))

unfoldlM :: (Monad m, Unfoldl 0 n n) => m a -> m (LengthR n a)
unfoldlM = (`unfoldlMWithBase` NilR)

unfoldlMWithBase ::
	(Monad m, Unfoldl n w w) => m a -> RangeR n w a -> m (LengthR w a)
unfoldlMWithBase = unfoldlMRangeWithBase undefined

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
