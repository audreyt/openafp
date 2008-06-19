
module OpenAFP.Records.AFP.PTD1 where
import OpenAFP.Types
import OpenAFP.Internals
 
data PTD1 = PTD1 {
    ptd1_Type                        :: !N3
    ,ptd1_                           :: !N3
    ,ptd1                            :: !NStr
} deriving (Show, Typeable)

