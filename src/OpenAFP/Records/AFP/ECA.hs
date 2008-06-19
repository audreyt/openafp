
module OpenAFP.Records.AFP.ECA where
import OpenAFP.Types
import OpenAFP.Internals
 
data ECA = ECA {
    eca_Type                         :: !N3
    ,eca_                            :: !N3
    ,eca                             :: !NStr
} deriving (Show, Typeable)

