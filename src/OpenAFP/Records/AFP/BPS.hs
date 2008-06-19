
module OpenAFP.Records.AFP.BPS where
import OpenAFP.Types
import OpenAFP.Internals
 
data BPS = BPS {
    bps_Type                         :: !N3
    ,bps_                            :: !N3
    ,bps                             :: !AStr
} deriving (Show, Typeable)

