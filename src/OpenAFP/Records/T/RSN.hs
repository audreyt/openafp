module OpenAFP.Records.T.RSN where
import OpenAFP.Types
import OpenAFP.Internals

data T_RSN = T_RSN {
    t_rsn_Type                       :: !N1
    ,t_rsn                           :: !NStr
} deriving (Show, Typeable)

