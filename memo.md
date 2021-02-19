memo
====

haddock
-------

### each modules

Refactor documents

* [x] Data.List.Length
	+ [x] LENGTHED LIST LEFT
		- [x] Type
			* [x] type LengthL
			* [x] data RangeL
		- [x] AddL
		- [x] Unfoldr
			* [x] class
			* [x] without monad
				+ [x] repeatL
				+ [x] fillL
				+ [x] unfoldr
				+ [x] unfoldrWithBase
			* [x] with monad
				+ [x] unfoldrM
				+ [x] unfoldrMWithBase
		- [x] ZipL
			* [x] zipL
			* [x] zipWithL
			* [x] zipWithML
		- [x] ListToLengthL
			* [x] listToLengthL
			* [x] chunksL
			* [x] chunksL'
	+ [x] LENGTHED LIST RIGHT
		- [x] Type
			* [x] type LengthR
			* [x] data RangeR
		- [x] AddR
		- [x] Unfoldl
			* [x] class
			* [x] without monad
				+ [x] repeatR
				+ [x] fillR
				+ [x] unfoldl
				+ [x] unfoldlWithBase
			* [x] with monad
				+ [x] unfoldlM
				+ [x] unfoldlMWithBase
		- [x] ZipR
			* [x] class
			* [x] zipR
			* [x] zipWithR
			* [x] zipWithMR
		- [x] ListToLengthR
			* [x] listToLengthR
			* [x] chunksR
			* [x] chunksR'
	+ [x] LEFT TO RIGHT
		- [x] (++.+)
		- [x] leftToRight
	+ [x] RIGHT TO LEFT
		- [x] (++..)
		- [x] rightToLeft
* [x] Data.List.Range
	+ [x] RANGED LIST LEFT
		- [x] Type
		- [x] PushL
		- [x] AddL
		- [x] LoosenLMin and LoosenLMax
			* [x] loosenL
			* [x] loosenLMin
			* [x] loosenLMax
		- [x] Unfoldr
			* [x] unfoldrRange
				+ [x] without monad
					- [x] unfoldrRange
					- [x] unfoldrRangeWithBase
					- [x] unfoldrRangeWithBaseWithS
				+ [x] with monad
					- [x] unfoldrMRange
					- [x] unfoldrMRangeWithBase
			* [x] unfoldrRangeMaybe
				+ [x] without monad
					- [x] unfoldrRangeMaybe
					- [x] unfoldrRangeMaybeWithBase
				+ [x] with monad
					- [x] unfoldrMRangeMaybe
					- [x] unfoldrMRangeMaybeWithBase
		- [x] ZipL
			* [x] zipL
			* [x] zipWithL
			* [x] zipWithML
		- [x] Repeat and Unfoldr Min and Max
			* [x] repeat
				+ [x] repeatLMin
				+ [x] repeatLMax
			* [x] unfoldr
				+ [x] unfoldrMin
				+ [x] unfoldrMax
			* [x] unfoldrM
				+ [x] unfoldrMMin
				+ [x] unfoldrMMax
	+ [x] RANGED LIST RIGHT
		- [x] Type
		- [x] PushR
		- [x] AddR
		- [x] LoosenRMin and LoosenRMax
			* [x] loosenR
			* [x] loosenRMin
			* [x] loosenRMax
		- [x] Unfoldl
			* [x] class
			* [x] unfoldlRange
				+ [x] without monad
					- [x] unfoldlRange
					- [x] unfoldlRangeWithBase
					- [x] unfoldlRangeWithBaseWithS
				+ [x] with monad
					- [x] unfoldlMRange
					- [x] unfoldlMRangeWithBase
			* [x] unfoldlRangeMaybe
				+ [x] without monad
					- [x] unfoldlRangeMaybe
					- [x] unfoldlRangeMaybeWithBase
				+ [x] with monad
					- [x] unfoldlMRangeMaybe
					- [x] unfoldlMRangeMaybeWithBase
		- [x] ZipR
			* [x] zipR
			* [x] zipWithR
			* [x] zipWithMR
		- [x] Repeat and Unfoldl Min and Max
			* [x] repeat
				+ [x] repeatRMin
				+ [x] repeatRMax
			* [x] unfoldl
				+ [x] unfoldlMin
				+ [x] unfoldlMax
			* [x] unfoldlM
				+ [x] unfoldlMMin
				+ [x] unfoldlMMakx
	+ [x] LEFT TO RIGHT
		- [x] (++.+)
		- [x] leftToRight
	+ [x] RIGHT TO LEFT
		- [x] (++..)
		- [x] rightToLeft
* [x] Data.List.Range.Nat
	+ [x] RangedNatL
		- [x] natL
		- [x] toIntL
		- [x] fromIntL
		- [x] splitAtL
	+ [x] RangedNatR
		- [x] natR
		- [x] toIntR
		- [x] fromIntR
		- [x] splitAtR

### README.md

* [ ] make sample codes
	+ [x] rectangle
	+ [x] LengthL
		- [x] Word64 converter
			* [x] remove word64r.hs
			* [x] unify word64.hs and Bit.hs
			* [x] select little endian or big endian
			* [x] refactoring
	+ [x] LengthR
		- [x] 4 points of rectangle
			* [x] class and instance of GetElems
			* [x] calculate 4 points
			* [x] refactoring
				+ [x] consider to rename `getElems`
				+ [x] consider to rename `GetElems`
				+ [x] rename `tryGetting` to `getRect`
				+ [x] refactor function `getRect`
				+ [x] rename `withTitles` to `titles`
				+ [x] refactor funrciotn `titles`
				+ [x] refactor function `fourPoints`
				+ [x] refactor function `printResult`
				+ [x] refactor `main`
				+ [x] others
			* [x] others
			* [ ] rename function `getRect`
	+ [ ] RangeL
		- [x] example to get a password
	+ [ ] Finger Tree
* [ ] make `README.md`
	+ [x] rectangle
	+ [x] Word64 converter
		- [x] header
		- [x] body
			* [x] function takeL
			* [x] data Bit
			* [x] function boolToBit
			* [x] function bitToNum63
			* [x] bitsToWord64
			* [x] takeWord64
			* [x] main and samples
	+ [x] LengthR
	+ [ ] 4 points of rectangle
		- [x] function `fourPoints`
		- [x] headers
		- [x] class and instance of `GetElems`
		- [x] explanation of `class GetElems n v`
		- [x] explanation of `instance GetElems 0 0`
		- [x] explanation of `instance GetElems n 0`
		- [x] explanation of `instance GetElems 0 v`
		- [x] explanation of `instance GetElems n v`
		- [x] try `getElems`
		- [x] function `titles`
		- [x] function `printResult`
		- [ ] function `getRect`
		- [ ] function `main`
		- [ ] others
	+ [ ] to get password
	+ [ ] Finger Tree

refactor
--------

* [x] consider move declaration of `type LengthL` to `Data.List.Range.RangeL`
* [ ] consider move declaration of `type LengthR` to `Data.List.Range.RangeR`
* [ ] refactor all
* [x] check module name hierarchy
* [x] refactor haddock
	+ [x] Data.List.Length
		- [x] LENGHTED LIST LEFT
			* [x] structure
			* [x] Type
			* [x] AddL
			* [x] Unfoldr
				+ [x] class
				+ [x] without monad
				+ [x] with monad
			* [x] ZipL
			* [x] ListToLengthL
		- [x] LENGTHED LIST RIGHT
			* [x] structure
			* [x] Type
			* [x] AddR
			* [x] Unfoldl
				+ [x] class
				+ [x] without monad
				+ [x] with monad
			* [x] ZipR
			* [x] ListToLengthR
		- [x] LEFT TO RIGHT
		- [x] RIGHT TO LEFT
	+ [x] Data.List.Range
		- [x] RANGED LIST LEFT
			* [x] structure
			* [x] Type
			* [x] PushL
			* [x] AddL
			* [x] LoosenLMin and LoosenLMax
				+ [x] loosenL
				+ [x] loosenLMin
				+ [x] loosenLMax
			* [x] Unfoldr
				+ [x] class
				+ [x] unfoldrRange
					- [x] without monad
					- [x] with monad
				+ [x] unfoldrRangeMaybe
					- [x] without monad
					- [x] with monad
			* [x] ZipL
			* [x] Repeat and Unfoldr Min and Max
				+ [x] repeat
				+ [x] unfoldr
				+ [x] unfoldrM
		- [x] RANGED LIST RIGHT
			* [x] structure
			* [x] Type
			* [x] PushR
			* [x] AddR
			* [x] LoosenRMin and LoosenRMax
				+ [x] loosenR
				+ [x] loosenRMin
				+ [x] loosenRMax
			* [x] Unfoldl
				+ [x] class
				+ [x] unfoldlRange
					- [x] without monad
					- [x] with monad
				+ [x] unfoldlRangeMaybe
					- [x] without monad
					- [x] with monad
			* [x] ZipR
			* [x] Repeat and Unfoldl Min and Max
				+ [x] repeat
				+ [x] unfoldl
				+ [x] unfoldlM
		- [x] LEFT TO RIGHT
		- [x] RIGHT TO LEFT
	+ [x] Data.List.Range.Nat
		- [x] structure
		- [x] RangedNatL
		- [x] RangedNatR
* [x] refactor with hlint
* [x] refactor Data.List.Range.RangeL
	+ [x] language extensions
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] TYPE
			* [x] RANGE LEFT
			* [x] INSTANCE FUNCTOR
			* [x] INSTANCE FOLDABLE
		- [x] PUSH
		- [x] ADD
		- [x] LOOSEN
			* [x] LOOSEN LEFT
			* [x] LOOSEN LEFT MIN
			* [x] LOOSEN LEFT MAX
		- [x] UNFOLDR
			* [x] CLASS
			* [x] INSTANCE
				+ [x] instance Unfoldr 0 0 0
				+ [x] instance Unfoldr 0 0 w
				+ [x] instance Unfoldr 0 v w
				+ [x] instance Unfoldr n v w
			* [x] UNFOLDR RANGE
				+ [x] unfoldrRange
				+ [x] unfoldrRangeWithBase
				+ [x] unfoldrRangeWithBaseWithS
				+ [x] unfoldrMRange
			* [x] UNFOLDR RANGE MAYBE
				+ [x] unfoldrRangeMaybe
				+ [x] unfoldrRangeMaybeWithBase
				+ [x] type St
				+ [x] unfoldrRangeMaybeWithBaseGen
				+ [x] unfoldrMRangeMaybe
		- [x] ZIP
			* [x] CLASS
				+ [x] class
				+ [x] instance ZipL 0 0 v w
				+ [x] instance ZipL 0 m v w
				+ [x] instance ZipL n m v w
			* [x] FUNCTION
				+ [x] zipL
				+ [x] zipWithL
* [x] refactor Data.List.Range.RangeR
	+ [x] language extensions
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] TYPE
			* [x] RANGE RIGHT
			* [x] INSTANCE FUNCTOR
			* [x] INSTANCE FOLDABLE
		- [x] PUSH
		- [x] ADD
		- [x] LOOSEN
			* [x] LOOSEN RIGHT
			* [x] LOOSEN RIGHT MIN
			* [x] LOOSEN RIGHT MAX
		- [x] UNFOLDL
			* [x] CLASS
			* [x] INSTANCE
				+ [x] instance Unfoldl 0 0 0
				+ [x] instance Unfoldl 0 0 w
				+ [x] instance Unfoldl 0 v w
				+ [x] instance Unfoldl n v w
			* [x] UNFOLDL RANGE
				+ [x] unfoldlRange
				+ [x] unfoldlRangeWithBase
				+ [x] unfoldlRangeWithBaseWithS
				+ [x] unfoldlMRange
			* [x] UNFOLDL RANGE MAYBE
				+ [x] unfoldlRangeMaybe
				+ [x] unfoldlRangeMaybeWithBase
				+ [x] type St
				+ [x] unfoldlRangeMaybeWithBaseGen
				+ [x] unfoldlMRangeMaybe
		- [x] ZIP
			* [x] CLASS AND INSTANCE
				+ [x] class ZipR
				+ [x] instance ZipR n m 0 0
				+ [x] instance ZipR n m 0 w
				+ [x] instance ZipR n m v w
			* [x] FUNCTION
				+ [x] zipR
				+ [x] zipWithR
* [x] refactor Data.List.Range
	+ [x] language extension
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] RANGED LIST LEFT
			* [x] MIN
			* [x] MAX
		- [x] RANGED LIST RIGHT
			* [x] MIN
			* [x] MAX
		- [x] LEFT TO RIGHT
			* [x] CLASS
			* [x] INSTANCE
				+ [x] LeftToRight n m 0 0
				+ [x] LeftToRight n m 0 w
				+ [x] LeftToRight n m v w
			* [x] FUNCTION
		- [x] RIGHT TO LEFT
			* [x] CLASS
			* [x] INSTANCE
				+ [x] RightToLeft 0 0 v w
				+ [x] RightToLeft 0 m v w
				+ [x] RightToLeft n m v w
			* [x] FUNCTION
* [x] refactor Data.List.Length.LengthL
	+ [x] language extension
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] TYPE
		- [x] UNFOLDR
		- [x] LIST TO LENGTH LEFT
* [x] refactor Data.List.Length.LengthR
	+ [x] language extension
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] TYPE
		- [x] UNFOLDL
		- [x] LIST TO LENGTH RIGHT
* [x] refactor Data.List.Length
	+ [x] language extension
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] LENGTH LEFT
		- [x] LENGTH RIGHT
* [x] refactor Data.List.Range.Nat
	+ [x] language extension
	+ [x] export list
	+ [x] import list
	+ [x] structure
	+ [x] body
		- [x] LENGED NAT LEFT
		- [x] LENGED NAT RIGHT
* [x] refactor Control.Monad.Identity
* [x] refactor Control.Monad.State
	+ [x] structure
	+ [x] body
		- [x] STATE LEFT
		- [x] STATE RIGHT
* [x] unify order of 'Min'/'Max' and 'M'
	+ unfoldrMMin, unfoldrMMax
	+ unfoldlMinM, unfoldlMaxM

todo
----

* [x] consider to use Unfoldr instead of UnfoldrMin and UnfoldrMax
* [x] consider to use Unfoldl instead of UnfoldlMin and UnfoldlMax
* [x] consider to add (xs0 :: RangeL n m a) argument to unfoldr
	+ `unfoldrWithBase :: (s -> (a, s)) -> s -> RangeL n m a -> LengthL m a`
* [x] consider to add (xs0 :: RangeR n m a) argument to unfoldl
	+ `unfoldlWithBase :: RangeR n m a -> (s -> (a, s)) -> s -> LengthR m a`
* [x] consider to add Data.List.Length.LengthL
* [x] consider to add Data.List.Length.LengthR
* [x] remove UnfoldrMin
* [x] remove UnfoldrMax
* [x] remove UnfoldlMin
* [x] remove UnfoldlMax
* [ ] repair Unfoldr
	+ add argument p :: s -> Bool
	+ move from module Foo.LengthX to Foo.RangeX
	+ [x] define Unfoldr'
	+ [x] define `unfoldrWithBaseRange :: RangeL n w a -> (s -> Bool) -> (s -> (a, s)) -> s -> (RangeL v w a, s)`
	+ [x] define `unfoldrWithBaseRangeM :: RangeL n w a -> (s -> Bool) -> (s -> m (a, s)) -> s -> m (RangeL v w a, s)`
	+ [x] move unfoldrWithBaseRange out of class
	+ [x] define `unfoldrWithBaseM'` with using `unfoldrWithBaseRangeM`
	+ [x] try to remove `class Unfoldr`
		- repair definitions
	+ [x] rename `class Unfoldr'` to `class Unfoldr`
	+ [x] rename `unfoldrWithBaseRange` to `unfoldrWithBaseRangeWithS`
	+ [x] rename `unfoldrWithBaseRangeM` to `unfoldrWithBaseRangeMWithS`
	+ [ ] repair definitions
	+ [ ] others
* [ ] repair Unfoldl
	+ [x] define class `Unfoldl'`
	+ [x] define instance `Unfoldl'`
		- [x] Unfoldl 0 0 0
		- [x] Unfoldl 0 0 w
		- [x] Unfoldl 0 v w
		- [x] Unfoldl n v w
	+ [x] define `unfoldlWithBaseM` with using `unfoldlWithBaseRangeMWithS`
	+ [x] try to remove `class Unfoldl`
		- repair definitions
	+ [x] rename `class Unfoldl'` to `class Unfoldl`
	+ [ ] others
* [x] define class ZipL
	+ [x] define `zipWithL`
	+ [x] define `zipWithML`
	+ [x] put `zipWithL` out of class
* [x] define class ZipR
	+ [x] define `zipWithR`
		- [x] define class
		- [x] define instance
			* [x] ZipR n m 0 0
			* [x] ZipR n m 0 w
			* [x] ZipR n m v w
	+ [x] define `zipWithMR`
	+ [x] put `zipWithR` out of class
* [x] define `unfoldrWithBaseRangeMWithSMaybe`
	+ [x] define `unfoldrRangeMaybe`
	+ [x] define `unfoldrWithBaseRangeMaybe`
	+ [x] define `unfoldrWithBaseRangeMMaybe`
	+ [x] put `unfoldrRangeMaybe` out of class
	+ [x] put `unfoldrWithBaseRangeMaybe` out of class
	+ [x] remove Proxy
* [x] define `unfoldlWithBaseRangeMWithSMaybe`
	+ [x] define `unfoldlRangeMaybe`
	+ [x] define `unfoldlWithBaseRangeMaybe`
	+ [x] define `unfoldlWithBaseRangeMMaybe`
	+ [x] put `unfoldlRangeMaybe` out of class
	+ [x] put `unfoldlWithBaseRangeMaybe` out of class
	+ [x] remove Proxy
* [x] define splitAtL
	+ [x] make Data.List.Range.Nat
		- [x] define type RangedNatL and RangedNatR
		- [x] define toIntL and toIntR
	+ [x] type RangedNat n m = Ranged n m ()
	+ [x] define toNatural (= length)
	+ [x] try to define Ranged Natural with using type level natural
		- use repeatL and loosenL
	+ [x] try to define Ranged Natural with using value level natural
		- use unfoldrFoo
	+ [x] define fromInt
	+ [x] define splitAtL
* [x] define splitAtR
* [x] repair type of `unfoldrMRange`
	+ from `s -> Bool` to `s -> m Bool`
* [x] rename from `unfoldlRangeM` to `unfoldlMRange`
* [x] repair type of `unfoldlMRange`
	+ from `s -> Bool` to `s -> m Bool`
* [x] repair type of Unfoldr class function
	+ [x] make Unfoldr'
	+ [x] use Unfoldr'
		- [x] use in unfoldrRangeWithBaseWithS
		- [x] use in unfoldrWithBaseRangeMaybe
		- [x] use unfoldrMRangeWithBase' in some functions
		- [x] use unfoldrMRangeMaybeWithBase' in some functions
	+ [x] remove Unfoldr
	+ [x] rename Unfoldr' to Unfoldr
* [x] repair type of Unfoldl class function
	+ [x] make class Unfoldl'
	+ [x] make instance Unfoldl'
		- [x] instance Unfoldl' 0 0 0
		- [x] instance Unfoldl' 0 0 w
		- [x] instance Unfoldl' 0 v w
		- [x] instance Unfoldl' n v w
	+ [x] use Unfoldl'
		- [x] use in unfoldlRangeMaybe
		- [x] use in others
	+ [x] remove Unfoldl
	+ [x] rename Unfoldl' to Unfoldl
* [x] flip argument s for unfoldls
* [x] define unfoldl functions
	+ [x] unfoldlRangeWithBaseWithS
	+ [x] unfoldlRangeWithBase
	+ [x] unfoldlRange
	+ [x] unfoldlMRange
	+ [x] unfoldlMRangeMaybe
	+ [x] unfoldlRangeMaybeWithBase
* [x] add (<=) to RangeL and RangeR definitions
	+ [x] RangeL
	+ [x] RangeR
* [x] `instance Monad (LengthL n)`
	+ [x] `instance Monad (RangeL n n)`
	+ [x] `instance Monad (LengthL n)`
* [ ] `instance Monad (LengthR n)`
	+ [x] `instance Monad (RangeR n n)`
	+ [ ] `instance Monad (LengthR n)`
* [x] `instance Applicative (RangeL n m)`
* [x] `instance Applicative (RangeR n m)`
* [x] rename listToLengthL to splitL
* [ ] rename listToLengthR to splitR
* [ ] refactoring
* [ ] consider to correct type of unfoldrMRangeMaybe

module name hierarchy
---------------------

```
Control.Monad
  +- Identity
  +- State
Data.List.Length
  +- LengthL
  +- LengthR
Data.List.Range
  +- RangeL
  +- RangeR
  +- Nat
```

trush
-----

* [ ] define ranged natural
	+ define Bit
	+ add bit and plus two nats
	+ RangedNat n m
* [ ] define `splitAtL :: RangedNat n m -> RangeL v w a -> (RangeL n m a, RangeL (v - m) (w - n) a)`
