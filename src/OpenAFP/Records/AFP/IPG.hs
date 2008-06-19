
module OpenAFP.Records.AFP.IPG where
import OpenAFP.Types
import OpenAFP.Internals
 
data IPG = IPG {
    ipg_Type                         :: !N3
    ,ipg_                            :: !N3
    ,ipg                             :: !NStr
} deriving (Show, Typeable)

