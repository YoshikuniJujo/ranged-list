{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Control.Monad.Catch
import Data.List.Length

main :: IO ()
main = do
	r <- tryGetting
	print r
	loop r
	where
	loop :: LengthR 4 String -> IO ()
	loop xa@(xs :+ _) = getLine >>= \case
		"q" -> pure ()
		"d" -> do
			r <- (getElems @3 @1 xs $ getLine >>= \case
				"d" -> pure $ Just Delete
				l -> pure . Just $ Value l) `catch` \(_ :: NothingToDeleteException) -> do
					putStrLn "*** Nothing to delete."
					tryGetting
			print r
			loop r
		_ -> putStrLn "q or d" >> loop xa

tryGetting :: IO (LengthR 4 String)
tryGetting = getElems NilR
	(getLine >>=
		\case "d" -> pure $ Just Delete; l -> pure . Just $ Value l)
	`catch`
	\(_ :: NothingToDeleteException) -> do
		putStrLn "*** Nothing to delete."
		tryGetting

data DeleteOr a = Delete | Value a deriving Show
data NothingToDeleteException = NothingToDeleteException deriving Show
instance Exception NothingToDeleteException

class GetElems n v where
	getElems :: MonadThrow m =>
		LengthR n a -> m (Maybe (DeleteOr a)) -> m (LengthR (n + v) a)

instance GetElems 0 0 where getElems NilR _ = pure NilR

instance 1 <= n => GetElems n 0 where getElems xs@(_ :+ _) _ = pure xs

instance GetElems 1 (v - 1) => GetElems 0 v where
	getElems NilR gt = gt >>= \case
		Nothing -> getElems NilR gt
		Just Delete -> throwM NothingToDeleteException
		Just (Value x) -> getElems @1 @(v - 1) (NilR :+ x) gt

instance {-# OVERLAPPABLE #-}
	(1 <= n, GetElems (n - 1) (v + 1), GetElems (n + 1) (v - 1)) => GetElems n v where
	getElems xa@(xs :+ _) gt = gt >>= \case
		Nothing -> getElems xa gt
		Just Delete -> getElems @(n - 1) @(v + 1) xs gt
		Just (Value x) -> getElems @(n + 1) @(v - 1) (xa :+ x) gt
