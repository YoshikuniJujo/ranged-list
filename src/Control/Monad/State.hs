{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.State where

import Control.Arrow

newtype StateL s a = StateL { runStateL :: s -> (a, s) }

instance Functor (StateL s) where f `fmap` StateL k = StateL $ (f `first`) . k

instance Applicative (StateL s) where
	pure = StateL . (,)
	StateL kf <*> mx = StateL \s -> let (f, s') = kf s in runStateL (f <$> mx) s'

instance Monad (StateL s) where
	StateL k >>= f = StateL \s -> let (x, s') = k s in runStateL (f x) s'

newtype StateR s a = StateR { runStateR :: s -> (s, a) }

instance Functor (StateR s) where f `fmap` StateR k = StateR $ (f `second`) . k

instance Applicative (StateR s) where
	pure = StateR . flip (,)
	StateR kf <*> mx = StateR \s -> let (s', f) = kf s in runStateR (f <$> mx) s'

instance Monad (StateR s) where
	StateR k >>= f = StateR \s -> let (s', x) = k s in runStateR (f x) s'
