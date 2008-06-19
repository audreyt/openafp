module OpenAFP.Records.T.FCGCSGI where
import OpenAFP.Types
import OpenAFP.Internals

data T_FCGCSGI = T_FCGCSGI {
    t_fcgcsgi_Type                   :: !N1
    ,t_fcgcsgi                       :: !NStr
} deriving (Show, Typeable)

