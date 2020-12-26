memo
====

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
* [ ] define `unfoldlWithBaseRangeMWithSMaybe`
	+ [ ] define `unfoldlRangeMaybe`
	+ [ ] define `unfoldlWithBaseRangeMaybe`
	+ [ ] define `unfoldlWithBaseRangeMMaybe`
	+ [ ] put `unfoldlRangeMaybe` out of class
	+ [ ] put `unfoldlWithBaseRangeMaybe` out of class
	+ [ ] remove Proxy
* [ ] define splitAtL
	+ [ ] type RangedNat n m = Ranged n m ()
	+ [ ] define toNatural (= length)
	+ [ ] try to define Ranged Natural with using type level natural
		- use repeatL and loosenL
	+ [ ] try to define Ranged Natural with using value level natural
		- use unfoldrFoo
	+ [ ] others
* [ ] define splitAtR

trush
-----

* [ ] define ranged natural
	+ define Bit
	+ add bit and plus two nats
	+ RangedNat n m
* [ ] define `splitAtL :: RangedNat n m -> RangeL v w a -> (RangeL n m a, RangeL (v - m) (w - n) a)`
