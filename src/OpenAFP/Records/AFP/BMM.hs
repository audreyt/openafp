
module OpenAFP.Records.AFP.BMM where
import OpenAFP.Types
import OpenAFP.Internals
 
data BMM = BMM {
    bmm_Type                         :: !N3
    ,bmm_                            :: !N3
    ,bmm                             :: !NStr
} deriving (Show, Typeable)

