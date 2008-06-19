
module OpenAFP.Records.AFP.EPG where
import OpenAFP.Types
import OpenAFP.Internals
 
data EPG = EPG {
    epg_Type                         :: !N3
    ,epg_                            :: !N3
    ,epg                             :: !AStr
} deriving (Show, Typeable)

