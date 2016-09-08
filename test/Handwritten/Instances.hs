{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}

module Instances where

import Types
import Foreign.Storable.Generic


instance GStorable Flat where
    gsizeOf _ = 16
    galignment _ = 8
    gpeekByteOff ptr offset = Flat 
        <$> gpeekByteOff ptr offset
        <*> gpeekByteOff ptr (offset+8)
    gpokeByteOff ptr offset (Flat a b) =  
           gpokeByteOff ptr  offset    a
        >> gpokeByteOff ptr (offset+8) b

instance GStorable Nested where 
    gsizeOf _ = 16
    galignment _ = 8
    gpeekByteOff ptr offset = Nested <$> (Flat 
        <$> gpeekByteOff ptr offset
        <*> gpeekByteOff ptr (offset+8))
    gpokeByteOff ptr offset (Nested (Flat a b)) =  
           gpokeByteOff ptr  offset    a
        >> gpokeByteOff ptr (offset+8) b

instance GStorable Nested2 where
    gsizeOf _ = 16
    galignment _ = 8
    gpeekByteOff ptr offset = Nested2 <$> (Nested <$> (Flat 
        <$> gpeekByteOff ptr offset
        <*> gpeekByteOff ptr (offset+8)))
    gpokeByteOff ptr offset (Nested2 (Nested (Flat a b))) =  
           gpokeByteOff ptr  offset    a
        >> gpokeByteOff ptr (offset+8) b
