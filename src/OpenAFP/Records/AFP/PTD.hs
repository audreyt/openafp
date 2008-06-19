
module OpenAFP.Records.AFP.PTD where
import OpenAFP.Types
import OpenAFP.Internals
 
data PTD = PTD {
    ptd_Type                         :: !N3
    ,ptd_                            :: !N3
    ,ptd                             :: !NStr
} deriving (Show, Typeable)

