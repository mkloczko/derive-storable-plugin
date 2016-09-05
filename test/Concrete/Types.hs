{-#LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics

data Flat    = Flat    Int  Double  deriving (Generic)
data Nested  = Nested  Flat         deriving (Generic)
data Nested2 = Nested2 Nested       deriving (Generic)
