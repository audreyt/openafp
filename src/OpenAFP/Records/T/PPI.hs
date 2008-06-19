module OpenAFP.Records.T.PPI where
import OpenAFP.Types
import OpenAFP.Internals

data T_PPI = T_PPI {
    t_ppi_Type                       :: !N1
    ,t_ppi                           :: !NStr
} deriving (Show, Typeable)

