
module OpenAFP.Records.AFP.TLE where
import OpenAFP.Types
import OpenAFP.Internals

data TLE = TLE {
    tle_Type                         :: !N3
    ,tle_                            :: !N3
    ,tle_Chunks                      :: ![T_]
} deriving (Show, Typeable)

