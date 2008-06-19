module OpenAFP.Records.T.LDOPM where
import OpenAFP.Types
import OpenAFP.Internals

data T_LDOPM = T_LDOPM {
    t_ldopm_Type                     :: !N1
    ,t_ldopm                         :: !NStr
} deriving (Show, Typeable)

