
module OpenAFP.Records.AFP.BBC where
import OpenAFP.Types
import OpenAFP.Internals
 
data BBC = BBC {
    bbc_Type                         :: !N3
    ,bbc_                            :: !N3
    ,bbc                             :: !NStr
} deriving (Show, Typeable)

