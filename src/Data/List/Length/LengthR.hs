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
unfoldl f s = unfoldlWithBase f s NilR

unfoldlWithBase :: Unfoldl n m m => (s -> (s, a)) -> s -> RangeR n m a -> LengthR m a
unfoldlWithBase f s xs = snd $ runStateR (unfoldlMWithBase (StateR f) xs) s

unfoldlM :: (Monad m, Unfoldl 0 n n) => m a -> m (LengthR n a)
unfoldlM f = unfoldlMWithBase f NilR

unfoldlMWithBase :: (Monad m, Unfoldl n w w) => m a -> RangeR n w a -> m (LengthR w a)
unfoldlMWithBase = unfoldlMRangeWithBase undefined

class ListToLengthR n where
	listToLengthR :: [a] -> Either (RangeR 0 (n - 1) a) (LengthR n a, [a])

instance ListToLengthR 1 where
	listToLengthR [] = Left NilR
	listToLengthR (x : xs) = Right (NilR :+ x, xs)

instance {-# OVERLAPPABLE #-} (1 <= (n - 1), ListToLengthR (n - 1)) => ListToLengthR n where
	listToLengthR [] = Left NilR
	listToLengthR (x : xs) = case listToLengthR xs of
		Left ys -> Left $ ys :++ x
		Right (ys, xs') -> Right (ys :+ x, xs')
