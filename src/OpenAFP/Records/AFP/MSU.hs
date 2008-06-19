
module OpenAFP.Records.AFP.MSU where
import OpenAFP.Types
import OpenAFP.Internals
 
data MSU = MSU {
    msu_Type                         :: !N3
    ,msu_                            :: !N3
    ,msu                             :: !NStr
} deriving (Show, Typeable)

