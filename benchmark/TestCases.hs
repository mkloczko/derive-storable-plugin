{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, DerivingVia, CPP #-}
module TestCases where 

import GHC.Generics (Generic)
import Foreign.Storable.Generic
import Data.Int
import Data.Word
import Control.DeepSeq


data C1 = C1 Int32                 deriving (Show, Generic, NFData)
                                   deriving (Storable) via Generically C1

data C2 = C2 Int8 Int32 Int16      deriving (Show, Generic, NFData)
                                   deriving (Storable) via Generically C2

data C3 = C3 C2 Int64 C1           deriving (Show, Generic, NFData)
                                   deriving (Storable) via Generically C3

data C4 = C4 Double Int8 C3        deriving (Show, Generic, NFData)
                                   deriving (Storable) via Generically C4

data C5 = C5 Int32 C2 C4           deriving (Show, Generic, NFData)
                                   deriving (Storable) via Generically C5

c1_def = C1 3
c2_def = C2 3 10 8
c3_def = C3 c2_def 11000 c1_def
c4_def = C4 0.312 3 c3_def 
c5_def = C5 100 c2_def c4_def 


data C1hw = C1hw Int32                 deriving (Show,Generic, NFData)
data C2hw = C2hw Int8 Int32 Int16      deriving (Show,Generic, NFData)
data C3hw = C3hw C2hw Int64 C1hw       deriving (Show,Generic, NFData)
data C4hw = C4hw Double Int8 C3hw      deriving (Show,Generic, NFData)
data C5hw = C5hw Int32 C2hw C4hw       deriving (Show,Generic, NFData)

c1hw_def = C1hw 3
c2hw_def = C2hw 3 10 8
c3hw_def = C3hw c2hw_def 11000 c1hw_def
c4hw_def = C4hw 0.312 3 c3hw_def 
c5hw_def = C5hw 100 c2hw_def c4hw_def 

instance Storable C1hw where
    sizeOf                         _ = 4
    alignment                      _ = 4
    peekByteOff ptr off              = C1hw <$> (peekByteOff ptr off :: IO Int32) 
    pokeByteOff ptr off (C1hw v) = pokeByteOff ptr off v

instance Storable C2hw where
    sizeOf                         _ = 12
    alignment                      _ = 4
    peekByteOff ptr off              = C2hw 
        <$> (peekByteOff ptr off        :: IO Int8 ) 
        <*> (peekByteOff ptr (off + 4)  :: IO Int32) 
        <*> (peekByteOff ptr (off + 8)  :: IO Int16) 
                                       
    pokeByteOff ptr off (C2hw i8a i32 i16) = do
        pokeByteOff ptr  off       i8a
        pokeByteOff ptr (off + 4)  i32
        pokeByteOff ptr (off + 8)  i16

instance Storable C3hw where
    sizeOf              _ = 32
    alignment           _ = 8
    peekByteOff ptr off = C3hw 
        <$> (peekByteOff ptr off        :: IO C2hw ) 
        <*> (peekByteOff ptr (off + 16) :: IO Int64) 
        <*> (peekByteOff ptr (off + 24) :: IO C1hw) 
                                       
    pokeByteOff ptr off (C3hw c2hw i64 c1hw) = do
        pokeByteOff ptr  off        c2hw
        pokeByteOff ptr (off + 16)  i64
        pokeByteOff ptr (off + 24)  c1hw

instance Storable C4hw where
    sizeOf              _ = 48
    alignment           _ = 8
    peekByteOff ptr off   = C4hw 
        <$> (peekByteOff ptr  off       :: IO Double)
        <*> (peekByteOff ptr (off + 8)  :: IO Int8  )
        <*> (peekByteOff ptr (off + 16) :: IO C3hw  )
    pokeByteOff ptr off (C4hw dbl i8 c3hw) = do
        pokeByteOff ptr  off       dbl
        pokeByteOff ptr (off + 8)  i8
        pokeByteOff ptr (off + 16) c3hw

instance Storable C5hw where 
    sizeOf              _ = 64
    alignment           _ = 8
    peekByteOff ptr off   = C5hw 
        <$> (peekByteOff ptr  off       :: IO Int32)
        <*> (peekByteOff ptr (off + 4 ) :: IO C2hw )
        <*> (peekByteOff ptr (off + 16) :: IO C4hw )
    pokeByteOff ptr off (C5hw i32 c2hw c4hw) = do
        pokeByteOff ptr  off       i32
        pokeByteOff ptr (off + 4)  c2hw
        pokeByteOff ptr (off + 16) c4hw

#ifdef GSTORABLE_SUMTYPES

data S1 = S1a Int32 | S1b Int16    deriving (Show, Generic, GStorable, NFData)
data S2 = S2a Int32 Int64 Int8 | S2b S1 Int8 Double
        | S2c Int32 Int8 Int16 Int8 | S2d S1 S1
        deriving (Show, Generic, GStorable, NFData)

s1_def = S1a 54
s2_def = S2b s1_def 32 1.5


data S1hw = S1hwa Int32 | S1hwb Int16  deriving (Show,Generic, NFData)
data S2hw = S2hwa Int32 Int64 Int8 | S2hwb S1hw Int8 Double
          | S2hwc Int32 Int8 Int16 Int8 | S2hwd S1hw S1hw
          deriving (Show, Generic, NFData)

s1hw_def = S1hwa 54
s2hw_def = S2hwb s1hw_def 32 1.5

instance Storable S1hw where
    sizeOf                         _ = 8
    alignment                      _ = 4
    peekByteOff ptr off              = do 
        let peek1 = S1hwa <$> (peekByteOff ptr (off+4) :: IO Int32)
            peek2 = S1hwb <$> (peekByteOff ptr (off+4) :: IO Int16)
        tag <- peekByteOff ptr off :: IO Word8 
        case tag of
            0 -> peek1
            1 -> peek2
            _ -> error "S1hw : Bad tag"
    pokeByteOff ptr off (S1hwa v) = pokeByteOff ptr off (0 :: Word8) >> pokeByteOff ptr (off+4) v
    pokeByteOff ptr off (S1hwb v) = pokeByteOff ptr off (1 :: Word8) >> pokeByteOff ptr (off+4) v

instance Storable S2hw where
    sizeOf                         _ = 32
    alignment                      _ = 8
    peekByteOff ptr off              = do 
        let peek1 = S2hwa <$> (peekByteOff ptr (off+ 8) :: IO Int32)
                          <*> (peekByteOff ptr (off+16) :: IO Int64)
                          <*> (peekByteOff ptr (off+24) :: IO  Int8)

            peek2 = S2hwb <$> (peekByteOff ptr (off+ 8) :: IO S1hw)
                          <*> (peekByteOff ptr (off+16) :: IO Int8)
                          <*> (peekByteOff ptr (off+24) :: IO Double)

            peek3 = S2hwc <$> (peekByteOff ptr (off+ 8) :: IO Int32)
                          <*> (peekByteOff ptr (off+12) :: IO  Int8)
                          <*> (peekByteOff ptr (off+16) :: IO Int16)
                          <*> (peekByteOff ptr (off+20) :: IO  Int8)

            peek4 = S2hwd <$> (peekByteOff ptr (off+ 8) :: IO S1hw)
                          <*> (peekByteOff ptr (off+16) :: IO S1hw)

        tag <- peekByteOff ptr off :: IO Word8 
        case tag of
            0 -> peek1
            1 -> peek2
            2 -> peek3
            3 -> peek4
            _ -> error "S1hw : Bad tag"

    pokeByteOff ptr off (S2hwa a b c) = do 
        pokeByteOff ptr  off     (0 :: Word8) 
        pokeByteOff ptr (off+ 8)  a
        pokeByteOff ptr (off+16)  b
        pokeByteOff ptr (off+24)  c
    pokeByteOff ptr off (S2hwb a b c) = do
        pokeByteOff ptr  off     (1 :: Word8) 
        pokeByteOff ptr (off+ 8)  a
        pokeByteOff ptr (off+16)  b
        pokeByteOff ptr (off+24)  c
    pokeByteOff ptr off (S2hwc a b c d) = do
        pokeByteOff ptr  off     (2 :: Word8) 
        pokeByteOff ptr (off+ 8)  a
        pokeByteOff ptr (off+12)  b
        pokeByteOff ptr (off+16)  c
        pokeByteOff ptr (off+24)  d
    pokeByteOff ptr off (S2hwd a b)     = do
        pokeByteOff ptr  off     (3 :: Word8) 
        pokeByteOff ptr (off+ 8)  a
        pokeByteOff ptr (off+16)  b

#endif
