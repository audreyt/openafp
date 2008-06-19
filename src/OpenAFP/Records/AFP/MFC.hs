
module OpenAFP.Records.AFP.MFC where
import OpenAFP.Types
import OpenAFP.Internals
 
data MFC = MFC {
    mfc_Type                         :: !N3
    ,mfc_                            :: !N3
    ,mfc                             :: !NStr
} deriving (Show, Typeable)

