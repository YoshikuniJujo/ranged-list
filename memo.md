memo
====

todo
----

* [x] consider to use Unfoldr instead of UnfoldrMin and UnfoldrMax
* [ ] consider to use Unfoldl instead of UnfoldlMin and UnfoldlMax
* [x] consider to add (xs0 :: RangeL n m a) argument to unfoldr
	+ `unfoldrWithBase :: (s -> (a, s)) -> s -> RangeL n m a -> LengthL m a`
* [x] consider to add (xs0 :: RangeR n m a) argument to unfoldl
	+ `unfoldlWithBase :: RangeR n m a -> (s -> (a, s)) -> s -> LengthR m a`
* [x] consider to add Data.List.Length.LengthL
* [x] consider to add Data.List.Length.LengthR
* [x] remove UnfoldrMin
* [x] remove UnfoldrMax
* [x] remove UnfoldlMin
* [ ] remove UnfoldlMax
