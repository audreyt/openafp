
module OpenAFP.Records.AFP.BDI where
import OpenAFP.Types
import OpenAFP.Internals
 
data BDI = BDI {
    bdi_Type                         :: !N3
    ,bdi_                            :: !N3
    ,bdi                             :: !NStr
} deriving (Show, Typeable)

