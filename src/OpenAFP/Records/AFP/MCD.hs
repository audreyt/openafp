
module OpenAFP.Records.AFP.MCD where
import OpenAFP.Types
import OpenAFP.Internals
 
data MCD = MCD {
    mcd_Type                         :: !N3
    ,mcd_                            :: !N3
    ,mcd                             :: !NStr
} deriving (Show, Typeable)

