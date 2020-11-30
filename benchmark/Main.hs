{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE AllowAmbiguousTypes #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE CPP #-}

import Criterion.Main
import Criterion.Types
import TestCases
import TestCasesOptimized
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (mallocArray, peekArray, pokeArray)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Generic
import Foreign.Storable.Generic.Internal

import GHC.Generics (from, to)

import GHC.Exts

import Control.DeepSeq
import Data.Proxy


mallocFree :: forall a. (Storable a) => a -> IO ()
mallocFree a = do
    ptr <- mallocBytes $ (sizeOf a)
    free ptr

singularTests = 
--   [ bgroup "mallocfree" $ 
--      [ bgroup "Handwritten" $
--          [ bench "C1" $ nfIO (mallocFree c1hw_def)
--          , bench "C2" $ nfIO (mallocFree c2hw_def)
--          , bench "C3" $ nfIO (mallocFree c3hw_def)
--          , bench "C4" $ nfIO (mallocFree c4hw_def)
--          , bench "C5" $ nfIO (mallocFree c5hw_def)
--          ]
--      , bgroup "GStorable" $
--          [ bench "C1" $ nfIO (mallocFree c1_def)
--          , bench "C2" $ nfIO (mallocFree c2_def)
--          , bench "C3" $ nfIO (mallocFree c3_def)
--          , bench "C4" $ nfIO (mallocFree c4_def)
--          , bench "C5" $ nfIO (mallocFree c5_def)
--          ]
--      ]
   [ bgroup "sizeOf" $ 
       [ bgroup "Handwritten" $
           [ bench "C1" $ nf sizeOf c1hw_def
           , bench "C2" $ nf sizeOf c2hw_def
           , bench "C3" $ nf sizeOf c3hw_def
           , bench "C4" $ nf sizeOf c4hw_def
           , bench "C5" $ nf sizeOf c5hw_def
#ifdef GSTORABLE_SUMTYPES
           , bench "S1" $ nf sizeOf s1hw_def
           , bench "S2" $ nf sizeOf s2hw_def
#endif
           ]
       
       , bgroup "GStorable" $
           [ bench "C1" $ nf sizeOf c1_def
           , bench "C2" $ nf sizeOf c2_def
           , bench "C3" $ nf sizeOf c3_def
           , bench "C4" $ nf sizeOf c4_def
           , bench "C5" $ nf sizeOf c5_def
#ifdef GSTORABLE_SUMTYPES
           , bench "S1" $ nf sizeOf s1_def
           , bench "S2" $ nf sizeOf s2_def
#endif
           ]
       , bgroup "Optimized" $
           [ bench "C1" $ nf sizeOf c1o_def
           , bench "C2" $ nf sizeOf c2o_def
           , bench "C3" $ nf sizeOf c3o_def
           , bench "C4" $ nf sizeOf c4o_def
           , bench "C5" $ nf sizeOf c5o_def
#ifdef GSTORABLE_SUMTYPES
           , bench "S1" $ nf sizeOf s1o_def
           , bench "S2" $ nf sizeOf s2o_def
#endif
           ]
       ]
   , bgroup "alignment" $ 
       [ bgroup "Handwritten" $
           [ bench "C1" $ nf alignment c1hw_def
           , bench "C2" $ nf alignment c2hw_def
           , bench "C3" $ nf alignment c3hw_def
           , bench "C4" $ nf alignment c4hw_def
           , bench "C5" $ nf alignment c5hw_def
#ifdef GSTORABLE_SUMTYPES
           , bench "S1" $ nf alignment s1hw_def
           , bench "S2" $ nf alignment s2hw_def
#endif
           ]
       , bgroup "GStorable" $
           [ bench "C1" $ nf alignment c1_def
           , bench "C2" $ nf alignment c2_def
           , bench "C3" $ nf alignment c3_def
           , bench "C4" $ nf alignment c4_def
           , bench "C5" $ nf alignment c5_def
#ifdef GSTORABLE_SUMTYPES
           , bench "S1" $ nf alignment s1_def
           , bench "S2" $ nf alignment s2_def
#endif
           ]
       , bgroup "Optimized" $
           [ bench "C1" $ nf alignment c1o_def
           , bench "C2" $ nf alignment c2o_def
           , bench "C3" $ nf alignment c3o_def
           , bench "C4" $ nf alignment c4o_def
           , bench "C5" $ nf alignment c5o_def
#ifdef GSTORABLE_SUMTYPES
           , bench "S1" $ nf alignment s1o_def
           , bench "S2" $ nf alignment s2o_def
#endif
           ]
       ]
   , bgroup "peek" $
       [ bgroup "Handwritten" $
           [ env (malloc @C1hw) $ \ptr -> bench "C1" $ nfIO (peek ptr)
           , env (malloc @C2hw) $ \ptr -> bench "C2" $ nfIO (peek ptr)
           , env (malloc @C3hw) $ \ptr -> bench "C3" $ nfIO (peek ptr)
           , env (malloc @C4hw) $ \ptr -> bench "C4" $ nfIO (peek ptr)
           , env (malloc @C5hw) $ \ptr -> bench "C5" $ nfIO (peek ptr)
#ifdef GSTORABLE_SUMTYPES
           , env (malloc @S1hw) $ \ptr -> bench "S1" $ nfIO (peek ptr)
           , env (malloc @S2hw) $ \ptr -> bench "S2" $ nfIO (peek ptr)
#endif
           ]
       , bgroup "GStorable" $
           [ env (malloc @C1  ) $ \ptr -> bench "C1" $ nfIO (peek ptr)
           , env (malloc @C2  ) $ \ptr -> bench "C2" $ nfIO (peek ptr)
           , env (malloc @C3  ) $ \ptr -> bench "C3" $ nfIO (peek ptr)
           , env (malloc @C4  ) $ \ptr -> bench "C4" $ nfIO (peek ptr)
           , env (malloc @C5  ) $ \ptr -> bench "C5" $ nfIO (peek ptr)
#ifdef GSTORABLE_SUMTYPES
           , env (malloc @S1  ) $ \ptr -> bench "S1" $ nfIO (peek ptr)
           , env (malloc @S2  ) $ \ptr -> bench "S2" $ nfIO (peek ptr)
#endif
           ]
       , bgroup "Optimized" $
           [ env (malloc @C1O ) $ \ptr -> bench "C1" $ nfIO ((peekByteOff ptr 0) :: IO C1O)
           , env (malloc @C2O ) $ \ptr -> bench "C2" $ nfIO ((peekByteOff ptr 0) :: IO C2O)
           , env (malloc @C3O ) $ \ptr -> bench "C3" $ nfIO (peek ptr)
           , env (malloc @C4O ) $ \ptr -> bench "C4" $ nfIO (peek ptr)
           , env (malloc @C5O ) $ \ptr -> bench "C5" $ nfIO (peek ptr)
#ifdef GSTORABLE_SUMTYPES
           , env (malloc @S1O ) $ \ptr -> bench "S1" $ nfIO (peek ptr)
           , env (malloc @S2O ) $ \ptr -> bench "S2" $ nfIO (peek ptr)
#endif
           ]
       ] 
  , bgroup "poke" $
      [ bgroup "Handwritten" $     
          [ env malloc $ \ptr -> bench "C1" $ nfIO (poke ptr c1hw_def) 
          , env malloc $ \ptr -> bench "C2" $ nfIO (poke ptr c2hw_def)
          , env malloc $ \ptr -> bench "C3" $ nfIO (poke ptr c3hw_def)
          , env malloc $ \ptr -> bench "C4" $ nfIO (poke ptr c4hw_def)
          , env malloc $ \ptr -> bench "C5" $ nfIO (poke ptr c5hw_def)
#ifdef GSTORABLE_SUMTYPES
          , env malloc $ \ptr -> bench "S1" $ nfIO (poke ptr s1hw_def)
          , env malloc $ \ptr -> bench "S2" $ nfIO (poke ptr s2hw_def)
#endif
          ]
      , bgroup "GStorable" $
          [ env malloc $ \ptr -> bench "C1" $ nfIO (poke ptr c1_def) 
          , env malloc $ \ptr -> bench "C2" $ nfIO (poke ptr c2_def)
          , env malloc $ \ptr -> bench "C3" $ nfIO (poke ptr c3_def)
          , env malloc $ \ptr -> bench "C4" $ nfIO (poke ptr c4_def)
          , env malloc $ \ptr -> bench "C5" $ nfIO (poke ptr c5_def)
#ifdef GSTORABLE_SUMTYPES
          , env malloc $ \ptr -> bench "S1" $ nfIO (poke ptr s1_def)
          , env malloc $ \ptr -> bench "S2" $ nfIO (poke ptr s2_def)
#endif
          ]
      , bgroup "Optimized" $
          [ env malloc $ \ptr -> bench "C1" $ nfIO (poke ptr c1o_def) 
          , env malloc $ \ptr -> bench "C2" $ nfIO (poke ptr c2o_def)
          , env malloc $ \ptr -> bench "C3" $ nfIO (poke ptr c3o_def)
          , env malloc $ \ptr -> bench "C4" $ nfIO (poke ptr c4o_def)
          , env malloc $ \ptr -> bench "C5" $ nfIO (poke ptr c5o_def)
#ifdef GSTORABLE_SUMTYPES
          , env malloc $ \ptr -> bench "S1" $ nfIO (poke ptr s1o_def)
          , env malloc $ \ptr -> bench "S2" $ nfIO (poke ptr s2o_def)
#endif
          ]
      ]
  ]



-- Our benchmark harness.
main = defaultMain $ singularTests

