
module OpenAFP.Records.AFP.EDG where
import OpenAFP.Types
import OpenAFP.Internals
 
data EDG = EDG {
    edg_Type                         :: !N3
    ,edg_                            :: !N3
    ,edg                             :: !NStr
} deriving (Show, Typeable)

