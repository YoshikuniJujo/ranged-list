{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Bit (Bit(..), bitToNum, boolToBit) where

data Bit = O | I deriving Show

bitToNum :: Num n => Bit -> n
bitToNum = \case O -> 0; I -> 1

boolToBit :: Bool -> Bit
boolToBit = \case False -> O; True -> I
