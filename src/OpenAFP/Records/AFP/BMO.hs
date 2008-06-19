
module OpenAFP.Records.AFP.BMO where
import OpenAFP.Types
import OpenAFP.Internals
 
data BMO = BMO {
    bmo_Type                         :: !N3
    ,bmo_                            :: !N3
    ,bmo                             :: !NStr
} deriving (Show, Typeable)

