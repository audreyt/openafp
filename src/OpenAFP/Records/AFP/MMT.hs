
module OpenAFP.Records.AFP.MMT where
import OpenAFP.Types
import OpenAFP.Internals
 
data MMT = MMT {
    mmt_Type                         :: !N3
    ,mmt_                            :: !N3
    ,mmt                             :: !NStr
} deriving (Show, Typeable)

