
module OpenAFP.Records.AFP.BPM where
import OpenAFP.Types
import OpenAFP.Internals
 
data BPM = BPM {
    bpm_Type                         :: !N3
    ,bpm_                            :: !N3
    ,bpm                             :: !NStr
} deriving (Show, Typeable)

