
module OpenAFP.Records.AFP.CPC where
import OpenAFP.Types
import OpenAFP.Internals
 
data CPC = CPC {
    cpc_Type                         :: !N3
    ,cpc_                            :: !N3
    ,cpc_GCGID                       :: !A8
    ,cpc_UseFlags                    :: !N1
    ,cpc_CPIRepeatingGroupLength     :: !N1
    ,cpc_SpaceCharacterSection       :: !N1
    ,cpc_UseFlags2                   :: !N1
    ,cpc                             :: !NStr
} deriving (Show, Typeable)

