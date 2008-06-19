module OpenAFP.Records.T.MF where
import OpenAFP.Types
import OpenAFP.Internals

data T_MF = T_MF {
    t_mf_Type                        :: !N1
    ,t_mf                            :: !NStr
} deriving (Show, Typeable)

