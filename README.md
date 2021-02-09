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

Suppose you want to make a value which represent rectangle.
You have a number list.
The numbers are a left border, a top border, width and height of
rectangle in order.
The numbers of first rectangle are followed by the numbers of second rectangle,
and so on.

```haskell
import Data.Length.Length

data Rect = Rect {
	left :: Double, top :: Double,
	width :: Double, height :: Double } derivins Show

makeRect :: Length 4 Double -> Rect
makeRect (l :. t :. w :. h :. NilL) = Rect l t w h

main :: IO ()
main = print $ map makeRect . fst $ chunksL [3, 5, 15, 2, 8, 4, 1, 9, 3, 5]
```

### To take Word64 from bit list

### Finger Tree
