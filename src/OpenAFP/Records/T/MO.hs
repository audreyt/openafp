module OpenAFP.Records.T.MO where
import OpenAFP.Types
import OpenAFP.Internals

data T_MO = T_MO {
    t_mo_Type                        :: !N1
    ,t_mo                            :: !NStr
} deriving (Show, Typeable)

