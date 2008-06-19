
module OpenAFP.Records.AFP.MIO where
import OpenAFP.Types
import OpenAFP.Internals
 
data MIO = MIO {
    mio_Type                         :: !N3
    ,mio_                            :: !N3
    ,mio                             :: !NStr
} deriving (Show, Typeable)

