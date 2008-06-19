
module OpenAFP.Records.AFP.FNG where
import OpenAFP.Types
import OpenAFP.Internals
 
data FNG = FNG {
    fng_Type                         :: !N3
    ,fng_                            :: !N3
    ,fng                             :: !NStr
} deriving (Show, Typeable)

