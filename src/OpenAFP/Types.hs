{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
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
    viewString, viewNStr, viewAStr, viewField, viewRecord
) where
import OpenAFP.Types.Buffer
import OpenAFP.Types.Chunk
import OpenAFP.Types.Record
import OpenAFP.Types.View
import OpenAFP.Internals
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

data T_ = T_ !N1 !Buffer1 deriving (Show, Typeable)

viewChunks cs = ViewChunks (typeOf cs) (map viewChunk cs)

viewChunk c = withChunk c recView

withChunk :: (ChunkBuf a n b) => a -> (forall r. (Rec r) => r -> x) -> x
withChunk c = chunkApply (fst . chunkDecon $ c) c

viewData ds = ViewData (typeOf ds) (map recView ds)

viewNumber n = ViewNumber (typeOf n) (fromEnum n)

viewString a = ViewString (typeOf a) (S.reverse $ S.pack [ toAsc n | n <- [ 0..((sizeOf a)-1) ] ])
    where
    toAsc n = ebc2ascW8 ! fromIntegral ((a `shiftR` (8 * n)) .&. 0xFF)

viewNStr nstr = ViewNStr (typeOf nstr) (S.take 80 $ packBuf nstr)

viewAStr nstr = ViewString (typeOf nstr) (S.map (ebc2ascW8 !) (S.take 80 $ packBuf nstr))

viewField l content = ViewField (C.pack l) content

viewRecord = ViewRecord
