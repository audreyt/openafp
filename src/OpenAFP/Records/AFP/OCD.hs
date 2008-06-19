
module OpenAFP.Records.AFP.OCD where
import OpenAFP.Types
import OpenAFP.Internals
 
data OCD = OCD {
    ocd_Type                         :: !N3
    ,ocd_                            :: !N3
    ,ocd                             :: !NStr
} deriving (Show, Typeable)

