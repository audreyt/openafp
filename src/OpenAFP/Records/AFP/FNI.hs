
module OpenAFP.Records.AFP.FNI where
import OpenAFP.Types
import OpenAFP.Internals
 
data FNI = FNI {
    fni_Type                         :: !N3
    ,fni_                            :: !N3
    ,fni_Data                        :: ![Record FNI_Data]
} deriving (Show, Typeable)

data FNI_Data = FNI_Data {
     fni_GCGID                       :: !A8
    ,fni_CharacterIncrement          :: !N2
    ,fni_AscendHeight                :: !I2
    ,fni_DescendDepth                :: !I2
    ,fni_Reserved1                   :: !N2
    ,fni_FNMCount                    :: !N2
    ,fni_ASpace                      :: !I2
    ,fni_BSpace                      :: !N2
    ,fni_CSpace                      :: !I2
    ,fni_Reserved2                   :: !N2
    ,fni_BaseOffset                  :: !I2
} deriving (Show, Typeable)
 
