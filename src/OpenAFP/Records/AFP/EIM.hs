
module OpenAFP.Records.AFP.EIM where
import OpenAFP.Types
import OpenAFP.Internals
 
data EIM = EIM {
    eim_Type                         :: !N3
    ,eim_                            :: !N3
    ,eim                             :: !AStr
} deriving (Show, Typeable)

