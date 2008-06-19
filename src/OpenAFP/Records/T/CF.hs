module OpenAFP.Records.T.CF where
import OpenAFP.Types
import OpenAFP.Internals

data T_CF = T_CF {
    t_cf_Type                        :: !N1
    ,t_cf                            :: !NStr
} deriving (Show, Typeable)

