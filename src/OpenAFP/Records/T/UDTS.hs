module OpenAFP.Records.T.UDTS where
import OpenAFP.Types
import OpenAFP.Internals

data T_UDTS = T_UDTS {
    t_udts_Type                      :: !N1
    ,t_udts                          :: !NStr
} deriving (Show, Typeable)

