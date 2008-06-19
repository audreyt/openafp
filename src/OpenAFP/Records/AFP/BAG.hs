
module OpenAFP.Records.AFP.BAG where
import OpenAFP.Types
import OpenAFP.Internals

data BAG = BAG {
    bag_Type                         :: !N3
    ,bag_                            :: !N3
    ,bag                             :: !NStr
} deriving (Show, Typeable)

