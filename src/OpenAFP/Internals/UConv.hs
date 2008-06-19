{-# OPTIONS -#include <unicode/ucnv.h> #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Internals.UConv
-- Copyright   :  Audrey Tang
-- License     :  BSD-style
--
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (FFI extensions, UNIX98)
--
-- This module provides an interface to IBM ICU's uconv functionality
--
-----------------------------------------------------------------------------

-- Make with: ghc --make main.hs -L/usr/local/lib -licuuc -O -I/usr/local/include

module OpenAFP.Internals.UConv where

import Foreign
import Foreign.C
import Foreign.Ptr
import OpenAFP.Internals.Binary
import System.IO.Unsafe(unsafePerformIO)

-- |
-- Convert a sequence of bytes from one encoding to another.
uconv :: String         -- ^ The encoding to convert from
      -> String         -- ^ The encoding to convert to
      -> [N1]           -- ^ The byte sequence to convert
      -> Maybe String   -- ^ The byte sequence converted
uconv fromcode tocode bytes = unsafePerformIO $ do
  -- Create a conversion descriptor
    withCString fromcode    $ \fromcode_c -> do
    withCString tocode      $ \tocode_c -> do
    allocaBytes in_len      $ \in_c -> do
        pokeArray in_c bytes
        allocaBytes out_len $ \out_c -> do
        alloca              $ \err -> do
            poke err (0 :: Word8)
            len <- ucnv_convert tocode_c 
                                fromcode_c 
                                (castPtr    out_c)
                                (toEnum     out_len)
                                (castPtr    in_c)
                                (toEnum     in_len)
                                err
            errCode <- peek err :: IO Word8
            if (errCode == 0) then
                return . Just =<< peekCStringLen (out_c, fromEnum len)
                else 
                return Nothing -- ("<<error: " ++ (show errCode) ++ ">>")
    where
        out_len = (4 * in_len)
        in_len  = (length bytes)

foreign import ccall "ucnv_convert_3_4" ucnv_convert ::
    CString     -> -- toConverterName
    CString     -> -- fromConverterName
    Ptr CChar   -> -- target
    Int32       -> -- targetCapacity
    Ptr CChar   -> -- source
    Int32       -> -- sourceLength
    Ptr Word8   -> -- pErrorCode
    IO Int32       -- return: targetLength
