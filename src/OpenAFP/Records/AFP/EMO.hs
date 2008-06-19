
module OpenAFP.Records.AFP.EMO where
import OpenAFP.Types
import OpenAFP.Internals
 
data EMO = EMO {
    emo_Type                         :: !N3
    ,emo_                            :: !N3
    ,emo                             :: !NStr
} deriving (Show, Typeable)

