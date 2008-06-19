module OpenAFP.Records.T.FDS where
import OpenAFP.Types
import OpenAFP.Internals

data T_FDS = T_FDS {
    t_fds_Type                       :: !N1
    ,t_fds                           :: !NStr
} deriving (Show, Typeable)

