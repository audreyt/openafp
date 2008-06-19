
module OpenAFP.Records.AFP.IDD where
import OpenAFP.Types
import OpenAFP.Internals
 
data IDD = IDD {
    idd_Type                         :: !N3
    ,idd_                            :: !N3
    ,idd                             :: !NStr
} deriving (Show, Typeable)

