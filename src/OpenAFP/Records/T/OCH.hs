module OpenAFP.Records.T.OCH where
import OpenAFP.Types
import OpenAFP.Internals

data T_OCH = T_OCH {
    t_och_Type                       :: !N1
    ,t_och                           :: !NStr
} deriving (Show, Typeable)

