
module OpenAFP.Records.AFP.EOG where
import OpenAFP.Types
import OpenAFP.Internals
 
data EOG = EOG {
    eog_Type                         :: !N3
    ,eog_                            :: !N3
    ,eog                             :: !AStr
} deriving (Show, Typeable)

