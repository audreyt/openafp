module OpenAFP.Records.T.MOR where
import OpenAFP.Types
import OpenAFP.Internals

data T_MOR = T_MOR {
    t_mor_Type                       :: !N1
    ,t_mor                           :: !NStr
} deriving (Show, Typeable)

