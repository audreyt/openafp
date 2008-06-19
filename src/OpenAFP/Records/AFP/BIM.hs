
module OpenAFP.Records.AFP.BIM where
import OpenAFP.Types
import OpenAFP.Internals
 
data BIM = BIM {
    bim_Type                         :: !N3
    ,bim_                            :: !N3
    ,bim                             :: !AStr
} deriving (Show, Typeable)

