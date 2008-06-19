
module OpenAFP.Records.AFP.BFG where
import OpenAFP.Types
import OpenAFP.Internals
 
data BFG = BFG {
    bfg_Type                         :: !N3
    ,bfg_                            :: !N3
    ,bfg                             :: !NStr
} deriving (Show, Typeable)

