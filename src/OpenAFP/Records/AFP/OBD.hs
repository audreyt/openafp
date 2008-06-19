
module OpenAFP.Records.AFP.OBD where
import OpenAFP.Types
import OpenAFP.Internals
 
data OBD = OBD {
    obd_Type                         :: !N3
    ,obd_                            :: !N3
    ,obd                             :: !NStr
} deriving (Show, Typeable)

