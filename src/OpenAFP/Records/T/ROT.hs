module OpenAFP.Records.T.ROT where
import OpenAFP.Types
import OpenAFP.Internals

data T_ROT = T_ROT {
    t_rot_Type                       :: !N1
    ,t_rot                           :: !NStr
} deriving (Show, Typeable)

