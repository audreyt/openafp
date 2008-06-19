
module OpenAFP.Records.AFP.MCF where
import OpenAFP.Types
import OpenAFP.Internals

data MCF_ = MCF_ !N0 !Buffer2 deriving (Show, Typeable)

data MCF = MCF {
    mcf_Type                         :: !N3
    ,mcf_                            :: !N3
    ,mcf_Chunks                      :: ![MCF_]
} deriving (Show, Typeable)

