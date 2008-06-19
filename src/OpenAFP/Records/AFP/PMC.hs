
module OpenAFP.Records.AFP.PMC where
import OpenAFP.Types
import OpenAFP.Internals
 
data PMC = PMC {
    pmc_Type                         :: !N3
    ,pmc_                            :: !N3
    ,pmc                             :: !NStr
} deriving (Show, Typeable)

