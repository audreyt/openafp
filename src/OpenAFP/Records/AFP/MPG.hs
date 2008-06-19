
module OpenAFP.Records.AFP.MPG where
import OpenAFP.Types
import OpenAFP.Internals
 
data MPG = MPG {
    mpg_Type                         :: !N3
    ,mpg_                            :: !N3
    ,mpg                             :: !NStr
} deriving (Show, Typeable)

