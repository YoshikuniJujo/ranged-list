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
* [ ] repair Unfold classes
	+ add argument p :: s -> Bool
	+ move from module Foo.LengthX to Foo.RangeX
	+ [x] define Unfoldr'
	+ [x] define `unfoldrWithBaseRange :: RangeL n m a -> (s -> Bool) -> (s -> (a, s)) -> s -> Range v w a`
	+ [ ] define `unfoldrWithBaseRangeR :: RangeL n m a -> (s -> Bool) -> (s -> mnd (a, s)) -> s -> mnd (Range v w a)`
	+ [ ] move unfoldrWithBaseRange out of class
	+ [ ] others
* [ ] define ranged natural
	+ define Bit
	+ add bit and plus two nats
	+ RangedNat n m
