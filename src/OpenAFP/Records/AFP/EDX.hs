
module OpenAFP.Records.AFP.EDX where
import OpenAFP.Types
import OpenAFP.Internals
 
data EDX = EDX {
    edx_Type                         :: !N3
    ,edx_                            :: !N3
    ,edx                             :: !NStr
} deriving (Show, Typeable)

