
module OpenAFP.Records.AFP.ERG where
import OpenAFP.Types
import OpenAFP.Internals
 
data ERG = ERG {
    erg_Type                         :: !N3
    ,erg_                            :: !N3
    ,erg                             :: !NStr
} deriving (Show, Typeable)

