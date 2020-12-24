{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

module Data.List.Length.LengthR where

import GHC.TypeNats
import Control.Monad.Identity

import Data.List.Range.RangeR

type LengthR n = RangeR n n

unfoldl :: Unfoldl 0 n => (s -> (a, s)) -> s -> LengthR n a
unfoldl f s = unfoldlWithBase f s NilR

unfoldlWithBase :: Unfoldl n m => (s -> (a, s)) -> s -> RangeR n m a -> LengthR m a
unfoldlWithBase f s xs = runIdentity $ unfoldlWithBaseM (Identity . f) s xs

unfoldlM :: (Monad m, Unfoldl 0 n) => (s -> m (a, s)) -> s -> m (LengthR n a)
unfoldlM f s = unfoldlWithBaseM f s NilR

class Unfoldl n w where
	unfoldlWithBaseM :: Monad m =>
		(s -> m (a, s)) -> s -> RangeR n w a -> m (LengthR w a)

instance Unfoldl 0 0 where
	unfoldlWithBaseM _ _ NilR = pure NilR
	unfoldlWithBaseM _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} (1 <= m, Unfoldl 0 (m - 1)) => Unfoldl 0 m where
	unfoldlWithBaseM f s NilR = do
		(x, s') <- f s
		(:+ x) <$> unfoldlWithBaseM f s' NilR
	unfoldlWithBaseM f s (xs :++ x) = (:+ x) <$> unfoldlWithBaseM f s xs
	unfoldlWithBaseM _ _ _ = error "never occur"

instance {-# OVERLAPPABLE #-} Unfoldl (n - 1) (m - 1) => Unfoldl n m where
	unfoldlWithBaseM f s (xs :+ x) = (:+ x) <$> unfoldlWithBaseM f s xs
	unfoldlWithBaseM _ _ _ = error "never occur"

class ListToLengthR m where
	listToLengthR :: [a] -> Either (RangeR 0 (m - 1) a) (LengthR m a, [a])

instance ListToLengthR 1 where
	listToLengthR [] = Left NilR
	listToLengthR (x : xs) = Right (NilR :+ x, xs)

instance {-# OVERLAPPABLE #-} (1 <= (m - 1), ListToLengthR (m - 1)) => ListToLengthR m where
	listToLengthR [] = Left NilR
	listToLengthR (x : xs) = case listToLengthR xs of
		Left ys -> Left $ ys :++ x
		Right (ys, xs') -> Right (ys :+ x, xs')
