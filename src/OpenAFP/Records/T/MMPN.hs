module OpenAFP.Records.T.MMPN where
import OpenAFP.Types
import OpenAFP.Internals

data T_MMPN = T_MMPN {
    t_mmpn_Type                      :: !N1
    ,t_mmpn                          :: !NStr
} deriving (Show, Typeable)

