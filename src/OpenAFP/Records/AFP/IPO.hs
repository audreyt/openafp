
module OpenAFP.Records.AFP.IPO where
import OpenAFP.Types
import OpenAFP.Internals
 
data IPO = IPO {
    ipo_Type                         :: !N3
    ,ipo_                            :: !N3
    ,ipo                             :: !NStr
} deriving (Show, Typeable)

