module OpenAFP.Records.T.CS where
import OpenAFP.Types
import OpenAFP.Internals

data T_CS = T_CS {
    t_cs_Type                        :: !N1
    ,t_cs                            :: !NStr
} deriving (Show, Typeable)

