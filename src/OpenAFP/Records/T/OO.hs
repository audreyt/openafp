module OpenAFP.Records.T.OO where
import OpenAFP.Types
import OpenAFP.Internals

data T_OO = T_OO {
    t_oo_Type                        :: !N1
    ,t_oo                            :: !NStr
} deriving (Show, Typeable)

