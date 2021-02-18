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

## Example

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
Character \'*\' is 1 and character \'.\' is 0.

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

### To show 4 points of rectangles

### To get passwords

### Finger Tree
