memo
====

refactor
--------

* [x] check module name hierarchy
* [ ] refactor haddock
	+ [x] Data.List.Length
		- [x] LENGHTED LIST LEFT
			* [x] structure
			* [x] Type
			* [x] AddL
			* [x] Unfoldr
			* [x] ZipL
			* [x] ListToLengthL
		- [x] LENGTHED LIST RIGHT
			* [x] structure
			* [x] Type
			* [x] AddR
			* [x] Unfoldl
			* [x] ZipR
			* [x] ListToLengthR
		- [x] LEFT TO RIGHT
		- [x] RIGHT TO LEFT
	+ [ ] Data.List.Range
		- [ ] RANGED LIST LEFT
			* [x] structure
			* [x] Type
			* [x] PushL
			* [x] AddL
			* [x] LoosenLMin and LoosenLMax
				+ [x] loosenL
				+ [x] loosenLMin
				+ [x] loosenLMax
			* [ ] Unfoldr
				+ [x] class
				+ [ ] unfoldrRange
					- [ ] without monad
						* [x] rename unfoldrWithBaseRange to unfoldrRangeWithBase
						* [ ] others
					- [ ] with monad
						* [x] rename unfoldrMWithBaseRange to unfoldrMRangeWithBase
						* [ ] others
				+ [ ] unfoldrRangeMaybe
					- [ ] without monad
						* [ ] rename  unfoldrWithBaseRangeMaybe to unfoldrRangeMaybeWithBase
						* [ ] others
					- [ ] with monad
						* [ ] rename  unfoldrMWithBaseRangeMaybe to unfoldrMRangeMaybeWithBase
						* [ ] others
			* [ ] ZipL
			* [ ] Repeat and Unfoldr Min and Max
				+ [ ] repeat
				+ [ ] unfoldr
				+ [ ] unfoldrM
		- [ ] RANGED LIST RIGHT
		- [ ] LEFT TO RIGHT
		- [ ] RIGHT TO LEFT
	+ [ ] Data.List.Range.Nat
* [ ] refactor Data.List.Range.RangeL
	+ [ ] export list
	+ [ ] import list
	+ [ ] structure
	+ [ ] body
* [ ] refactor Data.List.Range.RangeR
* [ ] refactor Data.List.Range
* [ ] refactor Data.List.Length.LengthL
* [ ] refactor Data.List.Length.LengthR
* [ ] refactor Data.List.Range.Nat

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
* [ ] check to rename `WithBase`

module name hierarchy
---------------------

```
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
