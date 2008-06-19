module OpenAFP.Records.T.PV where
import OpenAFP.Types
import OpenAFP.Internals

data T_PV = T_PV {
    t_pv_Type                        :: !N1
    ,t_pv                            :: !NStr
} deriving (Show, Typeable)

