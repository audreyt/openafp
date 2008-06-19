
module OpenAFP.Records.AFP.BRG where
import OpenAFP.Types
import OpenAFP.Internals
 
data BRG = BRG {
    brg_Type                         :: !N3
    ,brg_                            :: !N3
    ,brg                             :: !NStr
} deriving (Show, Typeable)

