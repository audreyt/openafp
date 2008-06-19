
module OpenAFP.Records.AFP.EPS where
import OpenAFP.Types
import OpenAFP.Internals
 
data EPS = EPS {
    eps_Type                         :: !N3
    ,eps_                            :: !N3
    ,eps                             :: !AStr
} deriving (Show, Typeable)

