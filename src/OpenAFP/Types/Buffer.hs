{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types.Buffer
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module handles sized binary buffers.
--
-----------------------------------------------------------------------------

module OpenAFP.Types.Buffer where
import OpenAFP.Internals
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- | The Buf class represents buffers, with the leading bytes
--   representing its length.
class (Show a, Typeable a) => Buf a where
    {-# SPECIALIZE mkBuf :: BS -> Buffer0 #-}
    {-# SPECIALIZE mkBuf :: BS -> Buffer1 #-}
    {-# SPECIALIZE mkBuf :: BS -> Buffer2 #-}
    mkBuf :: BS -> a
    {-# SPECIALIZE packBuf :: Buffer0 -> BS #-}
    {-# SPECIALIZE packBuf :: Buffer1 -> BS #-}
    {-# SPECIALIZE packBuf :: Buffer2 -> BS #-}
    packBuf :: a -> BS

-- | Buffer0, being a simple BS, is of unlimited length.
newtype Buffer0 = Buf0 { fromBuf0 :: BS } deriving (Show, Typeable)

-- | Buffer1 uses one byte as length, hence is at most 254 bytes long.
newtype Buffer1 = Buf1 { fromBuf1 :: BS } deriving (Show, Typeable)

-- | Buffer2 uses two bytes; it can store up to 65535 bytes.
newtype Buffer2 = Buf2 { fromBuf2 :: BS } deriving (Show, Typeable)

instance Buf Buffer0 where
    mkBuf = Buf0
    packBuf = fromBuf0
instance Buf Buffer1 where
    mkBuf = Buf1
    packBuf = fromBuf1
instance Buf Buffer2 where
    mkBuf = Buf2
    packBuf = fromBuf2

instance Binary Buffer0 where
    put (Buf0 bs) = putByteString bs
    get = fmap (Buf0 . S.concat . L.toChunks) getRemainingLazyByteString

instance Binary Buffer1 where
    put (Buf1 bs) = do
        putWord8 $ toEnum (S.length bs) + 1
        putByteString bs
    get = do
        sz  <- getWord8
        bs  <- getByteString $ fromEnum sz - 1
        return (Buf1 bs)

instance Binary Buffer2 where
    put (Buf2 bs) = do
        putWord16be $ toEnum (S.length bs) + 2
        putByteString bs
    get = do
        sz  <- getWord16be
        bs  <- getByteString $ fromEnum sz - 2
        return (Buf2 bs)

