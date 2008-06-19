module OpenAFP.Records.T.OCL where
import OpenAFP.Types
import OpenAFP.Internals

data T_OCL = T_OCL {
    t_ocl_Type                       :: !N1
    ,t_ocl                           :: !NStr
} deriving (Show, Typeable)

