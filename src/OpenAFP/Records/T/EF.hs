module OpenAFP.Records.T.EF where
import OpenAFP.Types
import OpenAFP.Internals

data T_EF = T_EF {
    t_ef_Type                        :: !N1
    ,t_ef                            :: !NStr
} deriving (Show, Typeable)

