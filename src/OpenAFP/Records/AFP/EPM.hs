
module OpenAFP.Records.AFP.EPM where
import OpenAFP.Types
import OpenAFP.Internals
 
data EPM = EPM {
    epm_Type                         :: !N3
    ,epm_                            :: !N3
    ,epm                             :: !NStr
} deriving (Show, Typeable)

