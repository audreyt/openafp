
module OpenAFP.Records.AFP.EFM where
import OpenAFP.Types
import OpenAFP.Internals
 
data EFM = EFM {
    efm_Type                         :: !N3
    ,efm_                            :: !N3
    ,efm                             :: !NStr
} deriving (Show, Typeable)

