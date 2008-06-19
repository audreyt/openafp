module OpenAFP.Records.PTX.STC where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_STC = PTX_STC {
    ptx_stc_Type                     :: !N1
    ,ptx_stc                         :: !NStr
} deriving (Show, Typeable)

