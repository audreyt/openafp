module OpenAFP.Records.T.POCP where
import OpenAFP.Types
import OpenAFP.Internals

data T_POCP = T_POCP {
    t_pocp_Type                      :: !N1
    ,t_pocp                          :: !NStr
} deriving (Show, Typeable)

