
module OpenAFP.Records.AFP.MCC where
import OpenAFP.Types
import OpenAFP.Internals
 
data MCC = MCC {
    mcc_Type                         :: !N3
    ,mcc_                            :: !N3
    ,mcc                             :: !NStr
} deriving (Show, Typeable)

