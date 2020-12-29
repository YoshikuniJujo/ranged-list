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
