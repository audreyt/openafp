
module OpenAFP.Records.AFP.MPS where
import OpenAFP.Types
import OpenAFP.Internals
 
data MPS = MPS {
    mps_Type                         :: !N3
    ,mps_                            :: !N3
    ,mps                             :: !NStr
} deriving (Show, Typeable)

