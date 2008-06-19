module OpenAFP.Records.T.OSFO where
import OpenAFP.Types
import OpenAFP.Internals

data T_OSFO = T_OSFO {
    t_osfo_Type                      :: !N1
    ,t_osfo                          :: !NStr
} deriving (Show, Typeable)

