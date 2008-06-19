
module OpenAFP.Records.AFP.CFC where
import OpenAFP.Types
import OpenAFP.Internals
 
data CFC = CFC {
    cfc_Type                         :: !N3
    ,cfc_                            :: !N3
    ,cfc_CFIRepeatingGroupLength     :: !N1
    ,cfc                             :: !NStr
} deriving (Show, Typeable)

