{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types where

import GHC.Generics
import Foreign.Storable.Generic

data Flat    = Flat    Int Double deriving (Generic, GStorable)
data Nested  = Nested  Flat       deriving (Generic, GStorable) 
data Nested2 = Nested2 Nested     deriving (Generic, GStorable)

newtype NFlat    = NFlat    Flat    deriving (Generic)
newtype NNested  = NNested  Nested  deriving (Generic)
newtype NNested2 = NNested2 Nested2 deriving (Generic)

