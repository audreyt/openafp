{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types
-- Copyright   :  (c) Autrijus Tang 2004
-- License     :  BSD-style
-- 
-- Maintainer  :  autrijus@autrijus.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module imports and re-exports the fundamental types in the OpenAFP
-- framework: Buffer, ChunkBuf and Record.
--
-----------------------------------------------------------------------------

module OpenAFP.Types (
    module OpenAFP.Types.Buffer,
    module OpenAFP.Types.Chunk,
    module OpenAFP.Types.Record,
    module OpenAFP.Types.View,
    T_(..), viewChunks, viewData, viewNumber,
    viewString, viewNStr, viewField, viewRecord
) where
import OpenAFP.Types.Buffer
import OpenAFP.Types.Chunk
import OpenAFP.Types.Record
import OpenAFP.Types.View
import OpenAFP.Internals

newtype T_ = T_ (N1, Buffer1) deriving (Show, Typeable)

viewChunks cs = do
    rs <- mapM viewChunk cs
    return $ ViewChunks (typeOf cs, rs)

viewChunk c = withChunk c recView

withChunk :: (ChunkBuf a n b, MonadIO m) => a -> (forall r. (Rec r) => r -> m x) -> m x
withChunk c = chunkApply (fst . chunkDecon $ c) c

viewData ds = do
    rs <- mapM recView ds
    return $ ViewData (typeOf ds, rs)

viewNumber n = return $ ViewNumber (typeOf n, fromEnum n)

viewString a = return $ ViewString (typeOf a, [ toAsc n | n <- [ 0..((sizeOf a)-1) ] ])
    where toAsc n = ebc2asc ! fromIntegral ((a `shiftR` (8 * n)) .&. 0xFF)

viewNStr nstr = do
    let (pstr, len) = bufToPStrLen nstr
    withForeignPtr (castForeignPtr pstr) $ \cstr -> do
        ns <- peekArray (min len 80) cstr
        return $ ViewNStr (typeOf nstr, ns)

viewField l io = do
    content <- io
    return $ ViewField (l, content)

viewRecord t io = do
    fields <- sequence io
    return $ ViewRecord (t, fields)
