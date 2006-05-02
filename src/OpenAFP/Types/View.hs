{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types.View
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- Concrete data views on Chunks and Records.
--
-----------------------------------------------------------------------------

module OpenAFP.Types.View where
import OpenAFP.Types.Buffer
import OpenAFP.Internals

type ChunksType = TypeRep
type RecordType = TypeRep
type NumberType = TypeRep
type NStrType   = TypeRep
type StringType = TypeRep
type DataType   = TypeRep
type FieldLabel = String

-- newtype ViewChunks = ViewChunks (ChunksType, [ViewRecord])
newtype ViewRecord = ViewRecord (RecordType, [ViewField]) deriving (Show, Typeable)
newtype ViewField  = ViewField  (FieldLabel, ViewContent) deriving (Show, Typeable)
data ViewContent
    = ViewNumber (NumberType, Int)
    | ViewString (StringType, String)
    | ViewNStr   (NStrType,   [N1])
    | ViewChunks (ChunksType, [ViewRecord])
    | ViewData   (DataType,   [ViewRecord])
    deriving (Show, Typeable)

