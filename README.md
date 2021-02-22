# ranged-list

## What's this

This package provides lists whose lengths are determined by the type and
lists whose ranges of lengths are determined by the type.

```haskell
sample1 :: LengthL 3 Integer
sample1 = 1 :. 2 :. 3 :. NilL

sample2 :: LengthR 3 Integer
sample2 = NilR :+ 1 :+ 2 :+ 3

sample3 :: RangeL 2 5 Integer
sample3 = 1 :. 2 :. 3 :.. 4 :.. NilL

sample4 :: RangeR 2 5 Integer
sample4 = NilR :++ 1 :++ 2 :+ 3 :+ 4
```

`LengthL 3 Integer` and `LengthR 3 Integer` are lists who have just 3 `Integer`.
`RangeL 2 5 Integer` and `RangeR 2 5 Integer` are lists whose element numbers
are 2 at minimum and 5 at maximum.
`LengthL 3 Integer` and `RangeL 2 5 Integer` are
pushed or poped a element from left.
`LengthR 3 Integer` and `RangeR 2 5 Integer` are
pushed or poped a element from right.

## Motivation

Suppose you want to take elements from list. You can use `take` like following.

```
xs = take 3 "Hello, world!"
```

The length of `xs` is lesser or equal `3`.
But you cannot use this knowledge when you write next code.
You should check the argument of a next function.

```haskell
fun :: [Char] -> ...
fun [] = ...
fun [x] = ...
fun [x, y] = ...
fun [x, y, z] = ...
fun _ = error "bad argument"
```
If you use `LengthL 3 Char`,
you don't need to mind the argument has more than 3 elements.

```haskell
fun :: LengthL 3 Char -> ...
fun (x :. y :. z :. NilL) = ...
```

## LengthL

### To make rectangles from a number list

Suppose you want to make a value which represent a rectangle.
You have a number list.
The numbers are a left border, a top border, a width and a height of
a rectangle in order.
The numbers of the first rectangle are followed by
the numbers of a second rectangle,
and the numbers of the second rectangle are followed by
the numbers of a third rectangle,
and so on.

```
[left1, top1, width1, height1, left2, top2, width2, height2, left3, ...]
```

The list of numbers defined above are covert to a following list.

```
[Rect left1 top1 width1 height1, Rect left2 top2 width2 height2, Rect left3 ...]
```

The code is following. (View `sample/rectangle.hs`)

```haskell:sample/rectangle.hs
import Data.Length.Length

data Rect = Rect {
	left :: Double, top :: Double,
	width :: Double, height :: Double } derivins Show

makeRect :: Length 4 Double -> Rect
makeRect (l :. t :. w :. h :. NilL) = Rect l t w h

main :: IO ()
main = print $ map makeRect . fst $ chunksL [3, 5, 15, 2, 8, 4, 1, 9, 3, 5]
```

The function `chunksL` return a value of type `([LengthL n a], RangeL 0 (n - 1) a)`.
The first value of this tuple is a list of `n` elements of type `a`.
And the second value of this tuple is rest elements.
The number of the rest elements is `0` at minimum and `n - 1` at maximum.

Try running.

```
% stack ghc sample/rectangle.hs
% ./sample/rectangle
[Rect {left = 3.0, top = 5.0, width = 15.0, height = 2.0},
Rect {left = 8.0, top = 4.0, width = 1.0, height = 9.0)}
```

### To take Word64 from bit list

Let's define function to take a 64 bit word from bit list. (View `sample/word64.hs`)
The language extensions and the import list are following.

```haskell:sample/word64.hs
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DAtaKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Foldable
import Data.List.Length
import Data.List.Range
import Data.Bits
import Data.Word
import Numeric
```

You define function `takeL` to take `n` elements from list.

```haskell:sample/word64.hs
takeL :: (LoosenLMax 0 (n - 1) n, Unfoldr 0 n n, ListToLengthL n) =>
	a -> [a] -> LengthL n a
takeL d = either ((`fillL` d) . loosenLMax) fst . splitL
```

The function `splitL` split a list and get n element lengthed list (`LengthL n a`) and a rest of the list.
If the list does not contain enough elements, then it returns a left value. It is a list of type `RangeL 0 (n - 1) a`.
The function `loosenLMax` convert the type `RangeL 0 (n - 1)` into `RangeL 0 n`.
And the function `fillL` fill the list with default value `d` to get a list `LengthL n a`.
Try it.

```
% stack ghci sample/word64.hs
> :set -XDataKinds
> takeL '@' "Hello, world!" :: LengthL 5 Char
'H' :. ('e' :. ('l' :. ('l' :. ('o' :. NilL))))
> takeL 'W' "Hi!" :: LengthL 5 Char
'H' :. ('i' :. ('!' :. ('@' :. ('@' :. NilL))))
```

You define data type which represent a bit as follow.

```haskell:sample/word64.hs
data Bit = O | I deriving Show

boolToBit :: Bool -> Bit
boolToBit = \case False -> O; True -> I

bitToNum63 :: (Num n, Bits n) => Bit -> n
bitToNum63 = \case O -> 0; I -> 1 `shiftL` 63
```

`O` is 0 and `I` is 1.
Function `boolToBit` converts a value of `Bool` into a value of `Bit`.
Function `bitToNum63` converts a value of `Bit` into a number.
It converte the bit as a 63rd bit.

You define the function which convert a bit list into 64 bit word.

```haskell:sample/word64.hs
bitsToWord64 :: LengthL 64 Bit -> Word64
bitsToWord64 = foldl' (\w b -> w `shiftR` 1 .|. bitToNum63 b) 0
```

It gets a bit from the left end.
It put the bit on a 63rd position of a 64 bit word.
Then it gets a next bit.
It shifts 64 bit word to the right.
And it put the bit on a 63rd position of a 64 bit word.
It continue in the same way.

You define the function which take 64 bit word from a bit list expressed
as string.

```haskell:sample/word64.hs
takeWord64 :: String -> Word64
takeWord64 = bitsToWord64 . takeL O . (boolToBit . (== '*') <$>)
```

The argument of this function is a string.
The string represent a bit sequence.
Character \'\*\' is 1 and character \'.\' is 0.

You define sample string and try it in function `main`.

```haskell:sample/word64.hs
sample1, sample2 :: String
sample1 = "...*..*..*...........*...**********...*************............******"
sample2 = "...*..*..*...........*.."

main :: IO ()
main = do
	putStrLn $ takeWord64 sample1 `showHex` ""
	putStrLn $ takeWord64 sample2 `showHex` ""
```

Try it.

```
% stack ghc sample/word64.hs
% ./sample/word64
8007ffc7fe200248
200248
```

## LengthR

### To push and pop from right

A value of the type `LengthR n a` is a list of values of the type `a`.
The length of the list is `n`.
And you can push and pop an element from right.
Try it. (view `sample/LengthR.hs`)

```haskell:sample/LengthR.hs
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LengthR where

import Data.List.Length

hello :: LengthR 5 Char
hello = NilR :+ 'h' :+ 'e' :+ 'l' :+ 'l' :+ 'o'
```

The value `hello` is a list of characters which length is `5`.
Let\'s push the character `'!'` from right.

```
% stack ghci sample/LengthR.hs
> hello
((((NilR :+ 'h') :+ 'e') :+ 'l') :+ 'l') :+ 'o'
> hello :+ '!'
(((((NilR :+ 'h') :+ 'e') :+ 'l') :+ 'l') :+ 'o') :+ '!'
```

### To show 4 points of rectangles

#### function `fourPoints` and headers

You want to calculate four points of rectangle
from the left-top point, width and height of the rectangle.
You define function `fourPoints`. (View `sample/fourPointsOfRect.hs`)

```haskell:sample/fourPointsOfRect.hs
fourPoints :: LengthR 4 Double -> LengthR 4 (Double, Double)
fourPoints (NilR :+ l :+ t :+ w :+ h) =
	NilR :+ (l, t) :+ (l + w, t) :+ (l, t + h) :+ (l + w, t + h)
```

You add language extensions and modules to import.

```haskell:sample/fourPointsOfRect.hs
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
```

Try it.

```
% stack ghci sample/fourPointsOfRect.hs
> fourPoints $ NilR :+ 300 :+ 200 :+ 50 :+ 30
(((NilR :+ (300.0,200.0)) :+ (350.0,200.0)) :+ (300.0,230.0)) :+ (350.0,230.0)
```

#### to input values interactively

You want to input values of a left bound, a top bound, a width and a height
interactively.
You want to delete the last value and reinput a new value.
First of all, you define two data type,
`DeleteOr a` and `NothingToDeleteException`.

```haskell:sample/fourPointsOfRect.hs
data DeleteOr a = Delete | Value a deriving Show
data NothingToDeleteException = NothingToDeleteException deriving Show
instance Exception NothingToDeleteException
```

And you define the function `getElems` as a class function.

```haskell:sample/fourPointsOfRect.hs
class GetElems n v where
	getElems :: MonadThrow m =>
		LengthR n a -> m (Maybe (DeleteOr a)) -> m (LengthR (n + v) a)

instance GetElems 0 0 where getElems NilR _ = pure NilR

instance {-# OVERLAPPABLE #-} 1 <= n => GetElems n 0 where
	getElems xs@(_ :+ _) _ = pure xs

instance {-# OVERLAPPABLE #-} GetElems 1 (v - 1) => GetElems 0 v where
	getElems NilR gt = gt >>= \case
		Nothing -> getElems NilR gt
		Just Delete -> throwM NothingToDeleteException
		Just (Value x) -> getElem @1 @(v - 1) (NilR :+ x) gt

instance {-# OVERLAPPABLE #-}
	(1 <= n, GetElems (n - 1) (v + 1), GetElems (n + 1) (v - 1)) =>
	GetElems n v where
	getElems xa@(xs :+ _) gt = gt >>= \case
		Nothing -> getElems xa gt
		Just Delete -> getElems @(n - 1) @(v + 1) xs gt
		Just (Value x) -> getElems @(n + 1) @(v - 1) (xa :+ x) gt
```

##### class GetElems n v

The class function `getElems` has two arguments.
The first argument is a list of values which are already inputed.
The second argument is a monad which returns 3 kinds of values,
a value which represents to delete, a new value to push to the list
or a value which represents to do nothing.

##### instance GetElems 0 0

`n == 0` and `v == 0` means that the function `getElems` get
 a list of no elements and return a list of no elements.

##### instance GetElems n 0

`v == 0` means that the function `getElems` get a list and
return the list as it is.

##### instance GetElems 0 v

`n == 0` means that there are no already inputed elements.
The monad returns 3 kind of values.
If it returns `Nothing`, then it rerun the whole as `getElems NilR gt`.
If it returns `Just Delete`, then `NothingToDeleteException` occurs.
If it returns `Just (Value x)`,
then it set the already-inputed elements to `NilR :+ x` and rerun the whole.

##### instance GetElems n v

The monad `gt` returns 3 kind of values.
If it returns `Nothing`, then rerun the whole as `getElems xa gt`.
If it returns `Just Delete`,
then it remove an element from the already-inputed list
and rerun the whole.
If it returns `Just (Value x)`,
then it set the already-inputed elements to `xa :+ x` and rerun the whole.

##### to try it

Try it.

```
% stack ghci sample/fourPointsOfRect.hs
> :set -XDataKinds -XBlockArguments -XLambdaCase
> getElems NilR (Just . Value <$> getLine) :: IO (LengthR 3 String)
foo
bar
baz
((NilR :+ "foo") :+ "bar") :+ "baz"
> gt = (<$> getLine) \case "" -> Nothing; "d' -> Just Delete; s -> Just (Value s)
> getElems NilR gt :: IO (LengthR 3 String)
foo
bar
d
boo

baz
((NilR :+ "foo") :+ "boo") :+ "baz"
> getElems NilR gt :: IO (LengthR 3 String)
foo
bar
d
d
hoge
piyo
baz
((NilR :+ "hoge") :+ "piyo") :+ "baz"
> getElems NilR gt :: IO (LengthR 3 String)
foo
bar
d
d
d
*** Exception: NothingToDeleteException
```

### function `titles`

You define the function `titles` which show values as string with title.

```haskell:sample/fourPointsOfRect.hs
titles :: (Show a, Applicative (LengthR n)) =>
	Int -> LengthR n String -> LengthR n a -> LengthR n String
titles n ts xs = (\t x -> t ++ replicate (n - length t) ' ' ++ ": " ++ show x)
	<$> ts <*> xs
```

Try it.

```
% stack ghci sample/fourPointsOfRect.hs
> titles 5 (NilR :+ "foo" :+ "bar" :+ "baz") (NilR :+ 123 :+ 456 :+ 789)
((NilR :+ "foo  : 123") :+ "bar  : 456") :+ "baz  : 789"
```

### function `printResult`

You define the function `printResult` which show values expressing a rectangle
and 4 points of rectangle.

```haskell:sample/fourPointsOfRect.hs
printResult :: LengthR 4 Double -> IO ()
printResult r = do
	putStrLn ""
	putStrLn `mapM_` titles 6 t r; putStrLn ""
	putStrLn `mapM_` titles 12 u (fourPoints r); putStrLn ""
	where
	t = NilR :+ "left :+ "top" :+ "width" :+ "height"
	u = NIlR :+ "left-top" :+ "right-top" :+ "left-bottom" :+ "right-bottom"
```

Try it.

```
% stack ghci sample/fourPointsOfRect.hs
> printResult $ NilR :+ 300 :+ 200 :+ 70 :+ 50

left  : 300.0
top   : 200.0
width : 70.0
height: 50.0

left-top    : (300.0,200.0)
right-top   : (370.0,200.0)
left-bottom : (300.0,250.0)
right-bottom: (370.0,250.0)
```

### function `getRect`

You define the function `getRect` which gets user input to make rectangle.

```haskell:sample/fourPointsOfRect.hs
getRect :: forall n . GetElems n (4 - n) =>
	LengthR n Double -> IO (LengthR 4 Double)
getRect xs = (<$) <$> id <*> printRect =<<
	getElems @n @(4 - n) xs ((<$> getLine) \case
		"d" -> Just Delete; l -> Value <*> readMaybe l)
	`catch`
	\(_ :: NothingToDeleteException) ->
		putStrLn *** Nothing to delete." >> getRect @0 NilR
```

It gets a user input with `getLine`.
If it is `"d"`, then it deletes the last input.
If there are nothing to delete, then `NothingToDeleteException` occur.
It catches this exception and shows error message and rerun `getRect`.

### function `main`

You define function `main`.

```haskell:sample/fourPointsOfRect.hs
main :: IO ()
main = getRect NilR >>= fix \go xa@(xs :+ _) -> getLine >>= \case
	"q" -> pure ()
	"d" -> go =<< getRect xs
	_ -> putStrLn "q or d" >> go xa
```

It call function `getRect` with list of `0` elements (`NilR`).
And it repeats function `getRect` with list of `4 - 1` elements (`xs`)
if you input `"d"`.

```
% stack ghc sample/fourPointsOfRect.hs
% ./sample/fourPointsOfRect
500
300
75
50

left  : 500.0
top   : 300.0
width : 75.0
height: 50.0

left-top    : (500.0,300.0)
right-top   : (575.0,300.0)
left-bottom : (500.0,350.0)
right-bottom: (575.0,350.0)

d
d
125
100

left  : 500.0
top   : 300.0
width : 125.0
height: 100.0

left-top    : (500.0,300.0)
right-top   : (625.0,300.0)
left-bottom : (500.0,400.0)
right-bottom: (625.0,400.0)

d
d
d
d
d
*** Nothing to delete.
2000
1500
90
50

left  : 2000.0
top   : 1500.0
width : 90.0
height: 50.0

left-top    : (2000.0,1500.0)
right-top   : (2090.0,1500.0)
left-bottom : (2000.0,1550.0)
right-bottom: (2090.0,1550.0)

q
```

## RangeL and RangeR

### To specify the range of a number of elements of a list

You can specify the range of a number of elements of a list.
There is a data type `RangeL n m a`.
It represents a list which have a type `a` element.
And its length is `n` at minimum and `m` at maximum.

```
% stack ghci
> :module Data.List.Range
> :set -XDataKinds
> 'h' :. 'e' :. 'l' :. 'l' :.. 'o' :.. NilL :: RangeL 3 8 Char
'h' :. ('e' :. ('l' :. ('l' :.. ('o' :.. NilL))))
```

### To get passwords

Suppose you want to get a password
whose length is 8 at minimum and 127 at maximum.
First of all, you define headers.

```haskell:sample/password.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.List.Range
import System.IO

import qualified Data.ByteString.Char8 as BSC
```

You define `type Password`.

```haskell:sample/password.hs
type Password = RangeL 8 127 Char
```

It is a list of `Char`.
Its length is 8 at minimum and 127 at maximum.

You define a function `getRangedString`.
It recieves a user input.
It return a just value if the length of the input is within range.
It return a nothing value if the length of the input is out of range.

```haskell:sample/password.hs
getRangedPassword :: Unfoldr 0 n m => IO (Maybe (RangeL n m Char))
getRangedPassword = do
	e <- hGetEcho stdin
	hSetEcho stdin False
	unfoldrMRangeMaybe ((/= '\n') <$> hLookAhead stdin) getChar
		<* hSetEcho stdin e
```

It makes echo of stdin off.
It gets characters until you input `'\n'`.
And it makes echo of stdin on.

```
% stack ghci sample/password.hs
> :set -XDataKinds
> getRangedPassword :: IO (Maybe Password)
(Input "foobarbaz")
Just ('f' :. ('o' :. ('o' :. ('b' :. ('a' :. ('r' :. ('b' :. ('a' :. ('z' :..NilL)))))))))
> getRangedPassword :: IO (Maybe Password)
(Input "foo")
Nothing
> getRangedPassword :: IO (Maybe (RangeL 2 5 Char))
(Input "foobar")
Nothing
> r
```

### Finger Tree
