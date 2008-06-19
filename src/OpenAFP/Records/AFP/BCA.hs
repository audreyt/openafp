
module OpenAFP.Records.AFP.BCA where
import OpenAFP.Types
import OpenAFP.Internals
 
data BCA = BCA {
    bca_Type                         :: !N3
    ,bca_                            :: !N3
    ,bca                             :: !NStr
} deriving (Show, Typeable)

