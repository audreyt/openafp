
module OpenAFP.Records.AFP.BR where
import OpenAFP.Types
import OpenAFP.Internals
 
data BR = BR {
    br_Type                          :: !N3
    ,br_                             :: !N3
    ,br                              :: !AStr
} deriving (Show, Typeable)

