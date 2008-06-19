
module OpenAFP.Records.AFP.BNG where
import OpenAFP.Types
import OpenAFP.Internals
 
data BNG = BNG {
    bng_Type                         :: !N3
    ,bng_                            :: !N3
    ,bng                             :: !NStr
} deriving (Show, Typeable)

