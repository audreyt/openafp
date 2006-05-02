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

-- | The Buf class represents buffers, with the leading bytes
--   representing its length.
class (Show a, Typeable a) => Buf a where
    bufFromPStrLen :: PStringLen -> a
    bufToPStrLen :: a -> PStringLen

-- | Buffer0, being a simple PStringLen, is of unlimited length.
newtype Buffer0 = Buf0 PStringLen deriving (Show, Typeable)

-- | Buffer1 uses one byte as length, hence is at most 254 bytes long.
newtype Buffer1 = Buf1 PStringLen deriving (Show, Typeable)

-- | Buffer2 uses two bytes; it can store up to 65535 bytes.
newtype Buffer2 = Buf2 PStringLen deriving (Show, Typeable)

instance Buf Buffer0 where
    bufFromPStrLen = Buf0
    bufToPStrLen (Buf0 b) = b
instance Buf Buffer1 where
    bufFromPStrLen = Buf1
    bufToPStrLen (Buf1 b) = b
instance Buf Buffer2 where
    bufFromPStrLen = Buf2
    bufToPStrLen (Buf2 b) = b

instance Binary Buffer0 where
    put bh buf = putBuf bh $ bufToPStrLen buf
    get bh = do
        pos     <- tellBin bh
        size    <- sizeBin bh
        pstrlen <- getBuf bh (size - pos)
	return $ bufFromPStrLen pstrlen

instance Binary Buffer1 where
    put bh buf = do
        put bh (fromIntegral (len + 1) :: N1)
        putBuf bh $ bufToPStrLen buf
        where len = snd $ bufToPStrLen buf
    get bh = do
        size <- get bh :: IO N1
        pstrlen <- getBuf bh $ (fromEnum size) - 1
	return $ bufFromPStrLen pstrlen

instance Binary Buffer2 where
    put bh buf = do
        put bh (fromIntegral (len + 2) :: N2)
        putBuf bh $ bufToPStrLen buf
        where len = snd $ bufToPStrLen buf
    get bh = do
        size <- get bh :: IO N2
        pstrlen <- getBuf bh $ (fromEnum size) - 2
	return $ bufFromPStrLen pstrlen
