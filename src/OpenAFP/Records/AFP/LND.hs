
module OpenAFP.Records.AFP.LND where
import OpenAFP.Types
import OpenAFP.Internals
 
data LND = LND {
    lnd_Type                         :: !N3
    ,lnd_                            :: !N3
    ,lnd                             :: !NStr
} deriving (Show, Typeable)

