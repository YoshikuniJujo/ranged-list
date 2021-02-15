{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Control.Monad.Fix
import Control.Monad.Catch
import Data.List.Length
import Text.Read

data DeleteOr a = Delete | Value a deriving Show
data NothingToDeleteException = NothingToDeleteException deriving Show
instance Exception NothingToDeleteException

class GetElems n v where
	getElems :: MonadThrow m =>
		LengthR n a -> m (Maybe (DeleteOr a)) -> m (LengthR (n + v) a)

instance GetElems 0 0 where getElems NilR _ = pure NilR

instance {-# OVERLAPPABLE #-} 1 <= n => GetElems n 0 where getElems xs@(_ :+ _) _ = pure xs

instance {-# OVERLAPPABLE #-} GetElems 1 (v - 1) => GetElems 0 v where
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

tryGetting :: forall n . (GetElems n (4 - n), GetElems 0 4) => LengthR n Double -> IO (LengthR (0 + 4) Double)
tryGetting xs = (<$) <$> id <*> printResult =<< getElems @n @(4 - n) xs
	(getLine >>=
		\case "d" -> pure $ Just Delete; l -> pure (Value <$> readMaybe l))
	`catch`
	\(_ :: NothingToDeleteException) -> do
		putStrLn "*** Nothing to delete."
		tryGetting @0 (NilR :: LengthR 0 Double)

withTitles :: (Show a, Applicative (LengthR n)) => Int -> LengthR n String -> LengthR n a -> LengthR n String
withTitles n ts xs = (\t v -> t ++ replicate (n - length t) ' ' ++ ": " ++ show v) <$> ts <*> xs

fourPoints :: LengthR 4 Double -> LengthR 4 (Double, Double)
fourPoints (NilR :+ l :+ t :+ w :+ h) =
	NilR :+ (l, t) :+ (l + w, t) :+ (l, t + h) :+ (l + w, t + h)

printResult :: LengthR 4 Double -> IO ()
printResult r = do
	putStrLn ""
	putStrLn `mapM_` withTitles 6 (NilR :+ "left" :+ "top" :+ "width" :+ "height") r
	putStrLn ""
	putStrLn `mapM_` withTitles 12 (NilR :+ "left-top" :+ "right-top" :+ "left-bottom" :+ "right-bottom") (fourPoints r)
	putStrLn ""

main :: IO ()
main = tryGetting NilR >>= fix \go xa@(xs :+ _) -> getLine >>= \case
	"q" -> pure ();
	"d" -> go =<< tryGetting xs
	_ -> putStrLn "q or d" >> go xa
