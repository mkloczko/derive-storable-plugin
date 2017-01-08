{-#LANGUAGE DeriveGeneric #-}

module Types where

import GHC.Generics

data Flat    = Flat    Int Double  deriving (Generic)
data Flat2   = Flat2   Double Char deriving (Generic)
data Nested  = Nested  Flat  Flat2 deriving (Generic)
data Nested2 = Nested2 Nested      deriving (Generic)

type TFlat    = Flat    
type TNested  = Nested  
type TNested2 = Nested2 
