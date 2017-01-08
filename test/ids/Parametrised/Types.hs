{-#LANGUAGE DeriveGeneric #-}

module Parametrised.Types where

import GHC.Generics

data Flat    a b = Flat    a b            deriving (Generic)
data Nested  a b = Nested  a (Flat a b)   deriving (Generic)
data Nested2 a b = Nested2 b (Nested a b) deriving (Generic)
