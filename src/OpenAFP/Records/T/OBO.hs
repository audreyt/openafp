module OpenAFP.Records.T.OBO where
import OpenAFP.Types
import OpenAFP.Internals

data T_OBO = T_OBO {
    t_obo_Type                       :: !N1
    ,t_obo                           :: !NStr
} deriving (Show, Typeable)

