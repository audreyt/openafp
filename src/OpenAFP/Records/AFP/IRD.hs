
module OpenAFP.Records.AFP.IRD where
import OpenAFP.Types
import OpenAFP.Internals
 
data IRD = IRD {
    ird_Type                         :: !N3
    ,ird_                            :: !N3
    ,ird_ImageData                   :: !NStr
} deriving (Show, Typeable)

