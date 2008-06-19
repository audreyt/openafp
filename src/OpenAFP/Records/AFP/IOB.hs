
module OpenAFP.Records.AFP.IOB where
import OpenAFP.Types
import OpenAFP.Internals
 
data IOB = IOB {
    iob_Type                         :: !N3
    ,iob_                            :: !N3
    ,iob                             :: !NStr
} deriving (Show, Typeable)

