
-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types.View
-- Copyright   :  (c) Audrey Tang 2004-2011
-- License     :  PublicDomain
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
import Data.ByteString

type ChunksType = TypeRep
type RecordType = TypeRep
type NumberType = TypeRep
type NStrType   = TypeRep
type StringType = TypeRep
type DataType   = TypeRep
type FieldLabel = ByteString

data ViewRecord = ViewRecord
    { vr_type   :: !RecordType
    , vr_field  :: ![ViewField]
    }
    deriving (Show, Typeable)

data ViewField = ViewField
    { vf_label   :: !FieldLabel
    , vf_content :: !ViewContent
    }
    deriving (Show, Typeable)

data ViewContent
    = ViewNumber { vc_type :: !NumberType, vc_number :: !Int }
    | ViewString { vc_type :: !StringType, vc_string :: !ByteString }
    | ViewNStr   { vc_type :: !NStrType,   vc_nstr   :: !ByteString }
    | ViewChunks { vc_type :: !ChunksType, vc_chunks :: ![ViewRecord] }
    | ViewData   { vc_type :: !DataType,   vc_data   :: ![ViewRecord] }
    deriving (Show, Typeable)

