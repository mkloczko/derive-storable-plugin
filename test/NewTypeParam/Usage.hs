{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
module Usage where

import Types
import Instances

import Foreign.Storable.Generic
import Foreign.Ptr

-----------
-- NFlat --
-----------

flatSizeOf :: NFlat Int Double -> Int
flatSizeOf = gsizeOf

flatAlignment :: NFlat Int Double -> Int
flatAlignment = galignmnent

flatPeekByteOff :: Ptr b -> Int -> IO (NFlat Int Double)
flatPeekByteOff = gpeekByteOff

-- flatPokeByteOff :: Ptr b -> Int -> NFlat Int Double -> IO ()
-- flatPokeByteOff = gpokeByteOff

-------------
-- NNested --
-------------

nestedSizeOf :: NNested Int Double -> Int
nestedSizeOf = gsizeOf

nestedAlignment :: NNested Int Double -> Int
nestedAlignment = galignmnent

nestedPeekByteOff :: Ptr b -> Int -> IO (NNested Int Double)
nestedPeekByteOff = gpeekByteOff

-- nestedPokeByteOff :: Ptr b -> Int -> NNested Int Double -> IO ()
-- nestedPokeByteOff = gpokeByteOff

---------------
-- NNested 2 --
---------------

nested2SizeOf :: NNested2 Int Double -> Int
nested2SizeOf = gsizeOf

nested2Alignment :: NNested2 Int Double -> Int
nested2Alignment = galignmnent

nested2PeekByteOff :: Ptr b -> Int -> IO (NNested2 Int Double)
nested2PeekByteOff = gpeekByteOff

-- nested2PokeByteOff :: Ptr b -> Int -> NNested2 Int Double -> IO ()
-- nested2PokeByteOff = gpokeByteOff
