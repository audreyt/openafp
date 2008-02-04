{-# OPTIONS -fglasgow-exts -O -funbox-strict-fields #-}

module OpenAFP.Internals.Binary (
    N0(..),

    N1(..), N2(..), N3(..), N4(..),
    I1(..), I2(..), I4(..), I8(..),
    A1(..), A2(..), A3(..), A4(..), A6(..), A8(..), A12(..),
    PStringLen, getList, putList, encodeList, decodeList, encodeListFile, decodeListFile,

    module Data.Binary,
    module Data.ByteString.Internal
  ) where

import Data.Binary
import Data.Array.Unboxed
import Data.Bits
import Data.Int
import Data.Word
import Data.Typeable
import Control.Monad            ( when, liftM )
import System.IO as IO
import System.IO.Error          ( mkIOError, eofErrorType )
import GHC.Exts
import GHC.Word                 ( Word8(..) )
import Foreign.Storable
import Data.Char
import Foreign.Ptr
import Numeric                  (showHex)

import System.Mem.Weak
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.ForeignPtr

import GHC.Base
import GHC.IOBase

import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString as S
import GHC.Word (Word32(..),Word16(..),Word64(..))
import qualified Data.ByteString.Lazy as L

newtype A1 = A1 Word8 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Bounded)
newtype A2 = A2 Word16 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Bounded)
newtype A3 = A3 Word32 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable)
newtype A4 = A4 Word32 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Bounded)
newtype A6 = A6 Word64 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable)
newtype A8 = A8 Word64 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Bounded)
newtype A12 = A12 Integer deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable)

newtype I1 = I1 Int8 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Ix, IArray UArray, Show)
newtype I2 = I2 Int16 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Ix, IArray UArray, Show)
newtype I4 = I4 Int32 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Ix, IArray UArray, Show)
newtype I8 = I8 Int64 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Ix, IArray UArray, Show)

newtype N1 = N1 { fromN1 :: Word8 } deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary, Ix, IArray UArray, Bounded)
newtype N2 = N2 { fromN2 :: Word16 } deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary, Bounded)
newtype N3 = N3 { fromN3 :: Word32 } deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable)
newtype N4 = N4 { fromN4 :: Word32 } deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary, Bounded)

data N0 = N0 deriving (Show, Ord, Enum, Eq, Typeable, Ix, Bounded)

instance Bounded A3 where
    minBound = A3 0
    maxBound = A3 (2 ^ (8 * 3))

instance Bounded A6 where
    minBound = A6 0
    maxBound = A6 (2 ^ (8 * 6))

instance Bounded A12 where
    minBound = A12 0
    maxBound = A12 (2 ^ (8 * 12))

instance Bounded N3 where
    minBound = N3 0
    maxBound = N3 (2 ^ (8 * 3))

instance Binary N0 where
    get = return N0
    put _ = return ()

instance Num N0 where
    x + _           = x
    x * _           = x
    abs x           = x
    signum x        = x
    fromInteger _   = N0

instance Show N1 where show = zeropad 1
instance Show N2 where show = zeropad 2
instance Show N3 where show = zeropad 3
instance Show N4 where show = zeropad 4

instance Show A1 where show = zeropad 1
instance Show A2 where show = zeropad 2
instance Show A3 where show = zeropad 3
instance Show A4 where show = zeropad 4
instance Show A6 where show = zeropad 6
instance Show A8 where show = zeropad 8
instance Show A12 where show = zeropad 12

{-# INLINE zeropad #-}
zeropad :: (Integral a) => Int -> a -> String
zeropad len str = replicate len' '0' ++ str'
    where str' = map toUpper $ showHex str ""
          len' = 2*len - length str'

instance Storable N3 where
    sizeOf w = 3
    alignment w = 8

instance Storable A3 where
    sizeOf w = 3
    alignment w = 8

instance Storable A6 where
    sizeOf w = 6
    alignment w = 8

instance Storable A12 where
    sizeOf w = 12
    alignment w = 8

instance (Storable a, Storable b) => Storable (a, b) where
    alignment _     = error "alignment not defined"
    sizeOf (x, y)   = sizeOf x + sizeOf y

-- type PStringLen = (ForeignPtr CChar, Int)
type PStringLen = S.ByteString

instance Binary A1 where
    put (A1 w) = putWord8 w
    get = fmap A1 getWord8

instance Binary A2 where
    put (A2 w) = putWord16be w
    get = fmap A2 getWord16be

instance Binary A4 where
    put (A4 w) = putWord32be w
    get = fmap A4 getWord32be

instance Binary A8 where
    put (A8 w) = putWord64be w
    get = fmap A8 getWord64be

instance Binary A3 where
    put (A3 w) = do
        putWord8 $ fromIntegral (shiftr_w32 w 16)
        putWord16be $ fromIntegral w
    get = do
        s <- readN 3 id
        return $! A3 ((fromIntegral (s `S.index` 0) `shiftl_w32` 16) .|.
                      (fromIntegral (s `S.index` 1) `shiftl_w32` 8) .|.
                      (fromIntegral (s `S.index` 2)))

instance Binary A6 where
    put (A6 w) = do
        putWord16be $ fromIntegral (shiftr_w64 w 32)
        putWord32be $ fromIntegral w
    get = do
        s <- readN 6 id
        return $! A6 ((fromIntegral (s `S.index` 0) `shiftl_w64` 40) .|.
                      (fromIntegral (s `S.index` 1) `shiftl_w64` 32) .|.
                      (fromIntegral (s `S.index` 2) `shiftl_w64` 24) .|.
                      (fromIntegral (s `S.index` 3) `shiftl_w64` 16) .|.
                      (fromIntegral (s `S.index` 4) `shiftl_w64` 8) .|.
                      (fromIntegral (s `S.index` 5)))

instance Binary A12 where
    put (A12 w) = do
        putWord32be $ fromIntegral (shiftR w 64)
        putWord64be $ fromIntegral w
    get = do
        s <- readN 12 id
        return $! A12 ((fromIntegral (s `S.index`  0) `shiftL` 88) .|.
                       (fromIntegral (s `S.index`  1) `shiftL` 80) .|.
                       (fromIntegral (s `S.index`  2) `shiftL` 72) .|.
                       (fromIntegral (s `S.index`  3) `shiftL` 64) .|.
                       (fromIntegral (s `S.index`  4) `shiftL` 56) .|.
                       (fromIntegral (s `S.index`  5) `shiftL` 48) .|.
                       (fromIntegral (s `S.index`  6) `shiftL` 40) .|.
                       (fromIntegral (s `S.index`  7) `shiftL` 32) .|.
                       (fromIntegral (s `S.index`  8) `shiftL` 24) .|.
                       (fromIntegral (s `S.index`  9) `shiftL` 16) .|.
                       (fromIntegral (s `S.index` 10) `shiftL`  8) .|.
                       (fromIntegral (s `S.index` 11)))

instance Binary I1 where
    put (I1 w) = putWord8 $ fromIntegral w
    get = fmap (I1 . fromIntegral) getWord8

instance Binary I2 where
    put (I2 w) = putWord16be $ fromIntegral w
    get = fmap (I2 . fromIntegral) getWord16be

instance Binary I4 where
    put (I4 w) = putWord32be $ fromIntegral w
    get = fmap (I4 . fromIntegral) getWord32be

instance Binary I8 where
    put (I8 w) = putWord64be $ fromIntegral w
    get = fmap (I8 . fromIntegral) getWord64be

instance Binary N3 where
    put (N3 w) = do
        putWord8 $ fromIntegral (shiftr_w32 w 16)
        putWord16be $ fromIntegral w
    get = do
        s <- readN 3 id
        return $! N3 ((fromIntegral (s `S.index` 0) `shiftl_w32` 16) .|.
                      (fromIntegral (s `S.index` 1) `shiftl_w32` 8) .|.
                      (fromIntegral (s `S.index` 2)))

------------------------------------------------------------------------
-- Unchecked shifts

shiftr_w16 :: Word16 -> Int -> Word16
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`   i)
shiftr_w32 :: Word32 -> Int -> Word32
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)

readN :: Int -> (S.ByteString -> a) -> Get a
readN n f = fmap f $ getBytes n
{-# INLINE readN #-}
-- ^ important

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32

shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)


shiftr_w64 = shiftR
shiftl_w64 = shiftL

getList :: Binary a => Get [a]
getList = do
    rv   <- isEmpty
    if rv then return [] else do
        x   <- get
        xs  <- getList
        return (x:xs)

putList :: Binary a => [a] -> Put
putList = mapM_ put

encodeList :: Binary a => [a] -> L.ByteString
encodeList = runPut . putList
{-# INLINE encodeList #-}

decodeList :: Binary a => L.ByteString -> [a]
decodeList = runGet getList


encodeListFile :: Binary a => FilePath -> [a] -> IO ()
encodeListFile f v = L.writeFile f (encodeList v)
decodeListFile :: Binary a => FilePath -> IO [a]
decodeListFile f = liftM decodeList (L.readFile f)
