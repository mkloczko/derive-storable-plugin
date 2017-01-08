{-# OPTIONS_GHC -fplugin Foreign.Storable.Generic.Plugin #-}
module Parametrised.Usage where

import Parametrised.Types
import Parametrised.Instances

import Foreign.Storable.Generic
import Foreign.Ptr
import Foreign.Marshal.Alloc

import GHC.Exts
----------
-- Flat --
----------

flatSizeOf :: Flat Int Double -> Int
flatSizeOf = gsizeOf

flatAlignment :: Flat Int Double -> Int
flatAlignment = galignment


flatPeekByteOff :: Ptr b -> Int -> IO (Flat Int Double)
flatPeekByteOff = gpeekByteOff



-- flatPokeByteOff :: Ptr b -> Int -> Flat Int Double -> IO ()
-- flatPokeByteOff = gpokeByteOff


-- ------------
-- -- Nested --
-- ------------
-- 
-- nestedSizeOf :: Nested Int Double -> Int
-- nestedSizeOf = gsizeOf
-- 
-- nestedAlignment :: Nested Int Double -> Int
-- nestedAlignment = galignment
-- 
-- nestedPeekByteOff :: Ptr b -> Int -> IO (Nested Int Double)
-- nestedPeekByteOff = gpeekByteOff
-- 
-- -- nestedPokeByteOff :: Ptr b -> Int -> Nested Int Double -> IO ()
-- -- nestedPokeByteOff = gpokeByteOff
-- 
-- --------------
-- -- Nested 2 --
-- --------------
-- 
-- nested2SizeOf :: Nested2 Int Double -> Int
-- nested2SizeOf = gsizeOf
-- 
-- nested2Alignment :: Nested2 Int Double -> Int
-- nested2Alignment = galignment
-- 
-- nested2PeekByteOff :: Ptr b -> Int -> IO (Nested2 Int Double)
-- nested2PeekByteOff = gpeekByteOff
-- 
-- -- nested2PokeByteOff :: Ptr b -> Int -> Nested2 Int Double -> IO ()
-- -- nested2PokeByteOff = gpokeByteOff
