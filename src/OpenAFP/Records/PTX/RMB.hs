module OpenAFP.Records.PTX.RMB where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_RMB = PTX_RMB {
    ptx_rmb_Type                     :: !N1
    ,ptx_rmb                         :: !N2
} deriving (Show, Typeable)

