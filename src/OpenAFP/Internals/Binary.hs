{-# OPTIONS -fglasgow-exts -O -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Internals.Binary
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- Binary IO library for buffers and file handles.
--
-- Based on Binary.hs from GHC source tree, (c) The University of Glasgow 2002.
--
-- Based on the nhc98 Binary library, which is copyright
-- (c) Malcolm Wallace and Colin Runciman, University of York, 1998.
-- Under the terms of the license for that software, we must tell you
-- where you can obtain the original version of the Binary library, namely
--     <http://www.cs.york.ac.uk/fp/nhc98/>
--
-----------------------------------------------------------------------------

module OpenAFP.Internals.Binary (
    {-class-} Binary(..),
    {-type-}  BinHandle,

    openBinIO, openBinIO_,
    openBinBuf, newBinBuf,

    seekBin, tellBin, closeBin, isEOFBin, sizeBin,

    putByte, getByte,

    getBuf, putBuf, bufOf,

    N0(..),

    N1(..), N2(..), N3(..), N4(..),
    I1(..), I2(..), I4(..), I8(..),
    A1(..), A2(..), A3(..), A4(..), A6(..), A8(..), A12(..),
    PStringLen,
  ) where

import Data.Array.Unboxed
import Data.Bits
import Data.Int
import Data.Word
import Data.Typeable
import Control.Monad            ( when )
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

---------------------------------------------------------------
--              BinHandle
---------------------------------------------------------------

data BinHandle =
    BinIO {             -- binary data stored in a file
        off_r   :: !IntM,         -- the current offset (cached)
        hdl     :: !IO.Handle           -- the file handle (must be seekable)
    }
    |
    BinBuf {            -- PStringLen based buffers
        off_r  :: !IntM,          -- the current offset (cached)
        buf    :: !PStringLen     -- the memory buffer
    }
    deriving (Show, Typeable)

type PStringLen = (ForeignPtr CChar, Int)

---------------------------------------------------------------
--              Accessors
---------------------------------------------------------------

bufOf :: BinHandle -> PStringLen
bufOf (BinIO _ _) = error "bufOf not defined"
bufOf (BinBuf _ buf) = buf

instance Show IntM where
    show i = (show $ unsafePerformIO $ readIntM i)

---------------------------------------------------------------
--              class Binary
---------------------------------------------------------------

class Binary a where
    put    :: BinHandle -> a -> IO ()
    get    :: BinHandle -> IO a

openBinIO_ :: IO.Handle -> IO BinHandle
openBinIO_ h = openBinIO h 

openBinIO :: IO.Handle -> IO BinHandle
openBinIO h = do
    r <- newIntM
    writeIntM r 0
    return (BinIO r h)

openBinBuf :: PStringLen -> IO BinHandle
openBinBuf pstrlen = do
   ix_r <- newIntM
   writeIntM ix_r 0
   return $ BinBuf ix_r pstrlen

newBinBuf :: Int -> IO BinHandle
newBinBuf size
    | size <= 0 = error "newBinBuf: size must be >= 0"
    | otherwise = do
        pstr <- mallocForeignPtrBytes size
        openBinBuf (pstr, size)

tellBin :: BinHandle -> IO Int
tellBin (BinIO  r _) = readIntM r
tellBin (BinBuf r _) = readIntM r

seekBin :: BinHandle -> Int -> IO ()
seekBin (BinIO ix_r h) p = do 
    writeIntM ix_r p
    hSeek h AbsoluteSeek (fromIntegral p)
seekBin (BinBuf ix_r _) p = do
    writeIntM ix_r p

closeBin :: BinHandle -> IO ()
closeBin (BinIO _ h) = hClose h
closeBin (BinBuf _ _) = return ()

isEOFBin :: BinHandle -> IO Bool
isEOFBin (BinIO _ h) = hIsEOF h
isEOFBin (BinBuf ix_r (_, len)) = do
    ix <- readIntM ix_r
    return (ix >= len)

sizeBin :: BinHandle -> IO Int
sizeBin (BinIO _ h) = do
    size <- hFileSize h
    return $ fromEnum size
sizeBin (BinBuf _ (_, len)) = return len

-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 (BinIO ix_r h) w = do
    -- ix <- readIntM ix_r
    hPutChar h $ chr $ fromEnum w   -- XXX not really correct
    -- writeIntM ix_r (ix+1)
    return ()
putWord8 (BinBuf ix_r (pstr, len)) w = do
    ix <- readIntM ix_r
    when (ix >= len) $
        ioError (mkIOError eofErrorType "putWord8" Nothing Nothing)
    writeIntM ix_r (ix+1)
    withForeignPtr pstr $ \cstr ->
        pokeByteOff cstr ix w
    return ()

getWord8 :: BinHandle -> IO Word8
getWord8 (BinIO ix_r h) = do
    ix <- readIntM ix_r
    c <- hGetChar h
    writeIntM ix_r (ix+1)
    return $! (fromIntegral (ord c))    -- XXX not really correct
getWord8 (BinBuf ix_r (pstr, len)) = do
    ix <- readIntM ix_r
--  when (ix >= len)  $
--      ioError (mkIOError eofErrorType "getWord8" Nothing Nothing)
    writeIntM ix_r (ix+1)
    withForeignPtr pstr $ \cstr ->
        peekByteOff cstr ix

getBuf :: BinHandle -> Int -> IO PStringLen
getBuf (BinIO ix_r h) size = do
    pstr <- mallocForeignPtrBytes size
    withForeignPtr pstr $ \cstr -> do
        len <- hGetBuf h cstr size
        ix <- readIntM ix_r
        writeIntM ix_r (ix + len)
        return (pstr, len)
getBuf (BinBuf ix_r (pstr, len)) size = do
    ix <- readIntM ix_r
    let len' = min (len - ix) size
    writeIntM ix_r (ix + len')
    withForeignPtr pstr $ \cstr -> do
        pstr' <- newForeignPtr_ (plusPtr cstr ix)
        addFinalizer pstr' $ touchForeignPtr pstr -- ???
        return (pstr', len')

putBuf :: BinHandle -> PStringLen -> IO ()
putBuf (BinIO ix_r h) (pstr, len) = do
    ix <- readIntM ix_r
    writeIntM ix_r (ix + len)
    withForeignPtr pstr $ \cstr ->
        hPutBuf h cstr len
putBuf (BinBuf ix_r (pstr, len)) (pstr', len') = do
    ix <- readIntM ix_r
    when ((ix + len') > len) $
        ioError (mkIOError eofErrorType "putBuf" Nothing Nothing)
    writeIntM ix_r (ix+len')
    withForeignPtr pstr $ \cstr ->
        withForeignPtr pstr' $ \cstr' ->
            copyBytes (plusPtr cstr ix) cstr' len'

putByte :: BinHandle -> Word8 -> IO ()
putByte bh w = put bh w

getByte :: BinHandle -> IO Word8
getByte = getWord8

-- -----------------------------------------------------------------------------
-- Primitve Word writes

instance Binary Word8 where
  put = putWord8
  get = getWord8

instance Binary Word16 where
  put h w = do -- XXX too slow.. inline putWord8?
    putByte h (fromIntegral (w `shiftR` 8))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 8) .|. fromIntegral w2)

newtype A1 = A1 Word8 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary)
newtype A2 = A2 Word16 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary)
newtype A3 = A3 Word32 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable)
newtype A4 = A4 Word32 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary)
newtype A6 = A6 Word64 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable)
newtype A8 = A8 Word64 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary)
newtype A12 = A12 Integer deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable)

newtype I1 = I1 Int8 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary, Ix, IArray UArray, Show)
newtype I2 = I2 Int16 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary, Ix, IArray UArray, Show)
newtype I4 = I4 Int32 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary, Ix, IArray UArray, Show)
newtype I8 = I8 Int64 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary, Ix, IArray UArray, Show)

newtype N1 = N1 Word8 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary, Ix, IArray UArray)
newtype N2 = N2 Word16 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary)
newtype N3 = N3 Word32 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable)
newtype N4 = N4 Word32 deriving (Ord, Enum, Real, Integral, Eq, Num, Bits, Typeable, Storable, Binary)

newtype N0 = N0 () deriving (Show, Ord, Enum, Eq, Typeable, Binary, Ix)

instance Num N0 where
    x + _           = x
    x * _           = x
    abs x           = x
    signum x        = x
    fromInteger _   = N0 ()

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
    sizeOf (x, y)   = (sizeOf x) + (sizeOf y)

instance Binary N3 where
  put h w = do
    putByte h (fromIntegral (w `shiftR` 16))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 16) .|. 
               (fromIntegral w2 `shiftL`  8) .|. 
               (fromIntegral w3))

instance Binary A3 where
  put h w = do
    putByte h (fromIntegral (w `shiftR` 16))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 16) .|. 
               (fromIntegral w2 `shiftL`  8) .|. 
               (fromIntegral w3))

instance Binary A6 where
  put h w = do
    putByte h (fromIntegral (w `shiftR` 40))
    putByte h (fromIntegral ((w `shiftR` 32) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 24) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    w5 <- getWord8 h
    w6 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 40) .|. 
               (fromIntegral w2 `shiftL` 32) .|. 
               (fromIntegral w3 `shiftL` 24) .|. 
               (fromIntegral w4 `shiftL` 16) .|. 
               (fromIntegral w5 `shiftL`  8) .|. 
               (fromIntegral w6))

instance Binary A12 where
  put h w = do
    putByte h (fromIntegral (w `shiftR` 88))
    putByte h (fromIntegral ((w `shiftR` 80) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 72) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 64) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 56) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 48) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 40) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 32) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 24) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    w5 <- getWord8 h
    w6 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 88) .|. 
               (fromIntegral w2 `shiftL` 80) .|. 
               (fromIntegral w3 `shiftL` 72) .|. 
               (fromIntegral w4 `shiftL` 64) .|. 
               (fromIntegral w5 `shiftL` 56) .|. 
               (fromIntegral w2 `shiftL` 48) .|. 
               (fromIntegral w3 `shiftL` 40) .|. 
               (fromIntegral w2 `shiftL` 32) .|. 
               (fromIntegral w3 `shiftL` 24) .|. 
               (fromIntegral w4 `shiftL` 16) .|. 
               (fromIntegral w5 `shiftL`  8) .|. 
               (fromIntegral w6))

instance Binary Word32 where
  put h w = do
    putByte h (fromIntegral (w `shiftR` 24))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 24) .|. 
               (fromIntegral w2 `shiftL` 16) .|. 
               (fromIntegral w3 `shiftL`  8) .|. 
               (fromIntegral w4))


instance Binary Word64 where
  put h w = do
    putByte h (fromIntegral (w `shiftR` 56))
    putByte h (fromIntegral ((w `shiftR` 48) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 40) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 32) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 24) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR`  8) .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    w5 <- getWord8 h
    w6 <- getWord8 h
    w7 <- getWord8 h
    w8 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 56) .|. 
               (fromIntegral w2 `shiftL` 48) .|. 
               (fromIntegral w3 `shiftL` 40) .|. 
               (fromIntegral w4 `shiftL` 32) .|. 
               (fromIntegral w5 `shiftL` 24) .|. 
               (fromIntegral w6 `shiftL` 16) .|. 
               (fromIntegral w7 `shiftL`  8) .|. 
               (fromIntegral w8))

-- -----------------------------------------------------------------------------
-- Primitve Int writes

instance Binary Int8 where
  put h w = put h (fromIntegral w :: Word8)
  get h    = do w <- get h; return $! (fromIntegral (w::Word8))

instance Binary Int16 where
  put h w = put h (fromIntegral w :: Word16)
  get h    = do w <- get h; return $! (fromIntegral (w::Word16))

instance Binary Int32 where
  put h w = put h (fromIntegral w :: Word32)
  get h    = do w <- get h; return $! (fromIntegral (w::Word32))

instance Binary Int64 where
  put h w = put h (fromIntegral w :: Word64)
  get h    = do w <- get h; return $! (fromIntegral (w::Word64))

-- -----------------------------------------------------------------------------
-- Instances for standard types

instance Binary () where
    put bh () = return ()
    get  _     = return ()

instance Binary Bool where
    put bh b    = putByte bh (fromIntegral (fromEnum b))
    get bh      = do x <- getWord8 bh; return $! (toEnum (fromEnum x))

instance Binary Char where
    put bh c    = put bh (fromIntegral (ord c) :: Word8)
    get bh      = do x <- get bh; return $! (chr (fromEnum (x :: Word8)))

-- XXX - 32-bitism
instance Binary Int where
    put bh i = put bh (fromIntegral i :: Int32)
    get bh = do
        x <- get bh
        return $! (fromIntegral (x :: Int32))

instance Binary a => Binary [a] where
    put bh list = do mapM_ (put bh) list
    get bh@(BinBuf ix_r (_, len)) = do
        ix <- readIntM ix_r
        if (ix >= len) then
            return []
            else do
            x  <- get bh
            xs <- get bh
            return (x:xs)
    get bh@(BinIO _ _) = (do
        x  <- get bh
        xs <- unsafeInterleaveIO $ get bh
        return (x:xs)) `catch` \e -> return []

instance (Binary a, Binary b) => Binary (a,b) where
    put bh (a,b)    = do put bh a; put bh b
    get bh          = do a <- get bh
                         b <- get bh
                         return (a,b)

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put bh (a,b,c)  = do put bh a; put bh b; put bh c
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         return (a,b,c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put bh (a,b,c,d)= do put bh a; put bh b; put bh c; put bh d
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         return (a,b,c,d)

---------------------------------------------------------------
--              Support code: IntM
---------------------------------------------------------------

data IntM = IntM (MutableByteArray# RealWorld)

newIntM :: IO IntM
newIntM = IO $ \s ->
    case newByteArray# size s of { (# s, arr #) ->
    (# s, IntM arr #) }
    where I# size = 4 -- XXX - 32-bitism

readIntM :: IntM -> IO Int
readIntM (IntM arr) = IO $ \s ->
    case readIntArray# arr 0# s of { (# s, i #) ->
    (# s, I# i #) }

writeIntM :: IntM -> Int -> IO ()
writeIntM (IntM arr) (I# i) = IO $ \s ->
    case writeIntArray# arr 0# i s of { s ->
    (# s, () #) }
