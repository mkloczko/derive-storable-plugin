{-#LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
module Types where

import GHC.Generics
import Foreign.Storable.Generic

data Flat    a b = Flat    a b            deriving (Generic, GStorable)
data Nested  a b = Nested  a (Flat a b)   deriving (Generic, GStorable)
data Nested2 a b = Nested2 b (Nested a b) deriving (Generic, GStorable)

newtype NFlat    a b = NFlat    (Flat    a b) deriving (Generic)
newtype NNested  a b = NNested  (Nested  a b) deriving (Generic)
newtype NNested2 a b = NNested2 (Nested2 a b) deriving (Generic)

