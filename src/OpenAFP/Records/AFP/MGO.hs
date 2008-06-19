
module OpenAFP.Records.AFP.MGO where
import OpenAFP.Types
import OpenAFP.Internals
 
data MGO = MGO {
    mgo_Type                         :: !N3
    ,mgo_                            :: !N3
    ,mgo                             :: !NStr
} deriving (Show, Typeable)

