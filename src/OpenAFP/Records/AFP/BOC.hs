
module OpenAFP.Records.AFP.BOC where
import OpenAFP.Types
import OpenAFP.Internals
 
data BOC = BOC {
    boc_Type                         :: !N3
    ,boc_                            :: !N3
    ,boc                             :: !NStr
} deriving (Show, Typeable)

