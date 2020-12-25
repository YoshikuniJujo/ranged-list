{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trush.Data.Natural.Range where

import GHC.TypeNats
import GHC.Natural

import Data.Proxy

data Bit = O | I deriving Show

data Bin :: Nat -> * where
	NoBit :: Bin 0
	(:^) :: Bin (n - 1) -> Bit -> Bin n

class BinToNatural n where binToNatural :: Bin n -> Natural

instance BinToNatural 0 where
	binToNatural NoBit = 0
	binToNatural _ = error "never occur"

instance {-# OVERLAPPABLE #-} BinToNatural (n - 1) => BinToNatural n where
	binToNatural (bs :^ O) = 2 * binToNatural bs
	binToNatural (bs :^ I) = 2 * binToNatural bs + 1
	binToNatural _ = error "never occur"

foo :: Bin n -> Proxy (2 ^ n)
foo _ = Proxy

data RangedNat :: Nat -> [Nat] -> * where
	Min :: RangedNat n '[]
	(:+) :: Bin i -> RangedNat n is -> RangedNat n (i ': is)

class RangedToNatural n is where rangedToNatural :: RangedNat n is -> Natural

instance RangedToNatural 0 '[] where rangedToNatural Min = 0

instance {-# OVERLAPPABLE #-}
	RangedToNatural (n - 1) '[] => RangedToNatural n '[] where
	rangedToNatural Min = rangedToNatural (Min :: RangedNat (n - 1) '[]) + 1
