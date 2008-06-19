
module OpenAFP.Records.AFP.IPD where
import OpenAFP.Types
import OpenAFP.Internals
 
data IPD = IPD {
    ipd_Type                         :: !N3
    ,ipd_                            :: !N3
    ,ipd                             :: !NStr
} deriving (Show, Typeable)

