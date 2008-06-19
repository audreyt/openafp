
module OpenAFP.Records.AFP.FNP where
import OpenAFP.Types
import OpenAFP.Internals
 
data FNP = FNP {
    fnp_Type                         :: !N3
    ,fnp_                            :: !N3
    ,fnp                             :: !NStr
} deriving (Show, Typeable)

