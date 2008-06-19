module OpenAFP.Records.T.PSRM where
import OpenAFP.Types
import OpenAFP.Internals

data T_PSRM = T_PSRM {
    t_psrm_Type                      :: !N1
    ,t_psrm                          :: !NStr
} deriving (Show, Typeable)

