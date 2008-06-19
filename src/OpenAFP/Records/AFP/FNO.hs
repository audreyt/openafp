
module OpenAFP.Records.AFP.FNO where
import OpenAFP.Types
import OpenAFP.Internals
 
data FNO = FNO {
    fno_Type                         :: !N3
    ,fno_                            :: !N3
    ,fno_Reserved                    :: !N2
    ,fno_CharacterRotation           :: !N2
    ,fno_MaxBaseOffset               :: !N2
    ,fno_MaxCharacterIncrement       :: !N2
    ,fno_SpaceCharacterIncrement     :: !N2
    ,fno                             :: !NStr
} deriving (Show, Typeable)

