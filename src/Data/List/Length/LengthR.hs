{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Length.LengthR where

import GHC.TypeNats
import Control.Monad.State

import Data.List.Range.RangeR

type LengthR n = RangeR n n

unfoldl :: Unfoldl 0 n n => (s -> (s, a)) -> s -> LengthR n a
unfoldl f s = unfoldlWithBase f NilR s

unfoldlWithBase :: Unfoldl n m m => (s -> (s, a)) -> RangeR n m a -> s -> LengthR m a
unfoldlWithBase f xs s = snd $ runStateR (unfoldlMWithBase (StateR f) xs) s

unfoldlM :: (Monad m, Unfoldl 0 n n) => m a -> m (LengthR n a)
unfoldlM f = unfoldlMWithBase f NilR

unfoldlMWithBase :: (Monad m, Unfoldl n w w) => m a -> RangeR n w a -> m (LengthR w a)
unfoldlMWithBase f = unfoldlMRangeWithBase undefined f

class ListToLengthR n where
	listToLengthR :: [a] -> Either (RangeR 0 (n - 1) a) (LengthR n a, [a])

instance ListToLengthR 1 where
	listToLengthR [] = Left NilR
	listToLengthR (x : xs) = Right (NilR :+ x, xs)

instance {-# OVERLAPPABLE #-} (1 <= (m - 1), ListToLengthR (m - 1)) => ListToLengthR m where
	listToLengthR [] = Left NilR
	listToLengthR (x : xs) = case listToLengthR xs of
		Left ys -> Left $ ys :++ x
		Right (ys, xs') -> Right (ys :+ x, xs')
