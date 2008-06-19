
module OpenAFP.Records.AFP.CTC where
import OpenAFP.Types
import OpenAFP.Internals
 
data CTC = CTC {
    ctc_Type                         :: !N3
    ,ctc_                            :: !N3
    ,ctc                             :: !NStr
} deriving (Show, Typeable)

