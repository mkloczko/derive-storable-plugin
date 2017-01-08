{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
{-# LANGUAGE FlexibleInstances #-}
module Instances where

import Types
import Foreign.Storable.Generic
import Foreign.Ptr 

instance (GStorable a, GStorable b) => GStorable (Flat    a b) where {
    {-# SPECIALIZE instance GStorable (Flat   Int Double)   #-};
    {-# SPECIALIZE instance GStorable (Flat   Float Double) #-}
}
instance (GStorable a, GStorable b) => GStorable (Nested  a b) where {
    {-# SPECIALIZE instance GStorable (Nested Int Double) #-};
    {-# SPECIALIZE instance GStorable (Nested Word Char)  #-}
}
instance (GStorable a, GStorable b) => GStorable (Nested2 a b) where {
    {-# SPECIALIZE instance GStorable (Nested2 Int Double)  #-};
    {-# SPECIALIZE instance GStorable (Nested2 Float Float) #-}
}
--instance (GStorable a, GStorable b) => GStorable (Nested2 a b)
