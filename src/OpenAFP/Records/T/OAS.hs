module OpenAFP.Records.T.OAS where
import OpenAFP.Types
import OpenAFP.Internals

data T_OAS = T_OAS {
    t_oas_Type                       :: !N1
    ,t_oas                           :: !NStr
} deriving (Show, Typeable)

