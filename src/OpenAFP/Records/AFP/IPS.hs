
module OpenAFP.Records.AFP.IPS where
import OpenAFP.Types
import OpenAFP.Internals
 
data IPS = IPS {
    ips_Type                         :: !N3
    ,ips_                            :: !N3
    ,ips                             :: !AStr
} deriving (Show, Typeable)

