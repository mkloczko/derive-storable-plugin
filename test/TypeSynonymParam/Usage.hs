{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
module Usage where

import Types
import Instances

import Foreign.Storable.Generic
import Foreign.Ptr

-----------
-- TFlat --
-----------

flatSizeOf :: TFlat Int Double -> Int
flatSizeOf = gsizeOf

flatAlignment :: TFlat Int Double -> Int
flatAlignment = galignmnent

flatPeekByteOff :: Ptr b -> Int -> IO (TFlat Int Double)
flatPeekByteOff = gpeekByteOff

-- flatPokeByteOff :: Ptr b -> Int -> TFlat Int Double -> IO ()
-- flatPokeByteOff = gpokeByteOff

-------------
-- TNested --
-------------

nestedSizeOf :: TNested Int Double -> Int
nestedSizeOf = gsizeOf

nestedAlignment :: TNested Int Double -> Int
nestedAlignment = galignmnent

nestedPeekByteOff :: Ptr b -> Int -> IO (TNested Int Double)
nestedPeekByteOff = gpeekByteOff

-- nestedPokeByteOff :: Ptr b -> Int -> TNested Int Double -> IO ()
-- nestedPokeByteOff = gpokeByteOff

---------------
-- TNested 2 --
---------------

nested2SizeOf :: TNested2 Int Double -> Int
nested2SizeOf = gsizeOf

nested2Alignment :: TNested2 Int Double -> Int
nested2Alignment = galignmnent

nested2PeekByteOff :: Ptr b -> Int -> IO (TNested2 Int Double)
nested2PeekByteOff = gpeekByteOff

-- nested2PokeByteOff :: Ptr b -> Int -> TNested2 Int Double -> IO ()
-- nested2PokeByteOff = gpokeByteOff
