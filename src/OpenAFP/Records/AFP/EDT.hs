
module OpenAFP.Records.AFP.EDT where
import OpenAFP.Types
import OpenAFP.Internals
 
data EDT = EDT {
    edt_Type                         :: !N3
    ,edt_                            :: !N3
    ,edt                             :: !NStr
} deriving (Show, Typeable)

