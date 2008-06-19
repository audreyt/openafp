
module OpenAFP.Records.AFP.EMM where
import OpenAFP.Types
import OpenAFP.Internals
 
data EMM = EMM {
    emm_Type                         :: !N3
    ,emm_                            :: !N3
    ,emm                             :: !NStr
} deriving (Show, Typeable)

