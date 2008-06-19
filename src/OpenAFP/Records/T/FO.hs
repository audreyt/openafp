module OpenAFP.Records.T.FO where
import OpenAFP.Types
import OpenAFP.Internals

data T_FO = T_FO {
    t_fo_Type                        :: !N1
    ,t_fo                            :: !NStr
} deriving (Show, Typeable)

