
module OpenAFP.Records.AFP.BDA where
import OpenAFP.Types
import OpenAFP.Internals
 
data BDA = BDA {
    bda_Type                         :: !N3
    ,bda_                            :: !N3
    ,bda                             :: !NStr
} deriving (Show, Typeable)

