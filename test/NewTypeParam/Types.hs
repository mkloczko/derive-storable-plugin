{-#LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics

data Flat    a b = Flat    a b
data Nested  a b = Nested  a (Flat a b) 
data Nested2 a b = Nested2 b (Nested a b)

newtype NFlat    a b = NFlat    (Flat    a b)
newtype NNested  a b = NNested  (Nested  a b)
newtype NNested2 a b = NNested2 (Nested2 a b)

