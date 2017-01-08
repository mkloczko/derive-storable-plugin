{-#LANGUAGE DeriveGeneric #-}

module TypeSynonymParam.Types where

import GHC.Generics

data Flat    a b = Flat    a b            deriving (Generic)
data Nested  a b = Nested  a (Flat a b)   deriving (Generic)
data Nested2 a b = Nested2 b (Nested a b) deriving (Generic)

type TFlat    a b = Flat    a b
type TNested  a b = Nested  a b
type TNested2 a b = Nested2 a b
