module OpenAFP.Records.T.TO where
import OpenAFP.Types
import OpenAFP.Internals

data T_TO = T_TO {
    t_to_Type                        :: !N1
    ,t_to                            :: !NStr
} deriving (Show, Typeable)

