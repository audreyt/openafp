module OpenAFP.Records.PTX.SVI where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_SVI = PTX_SVI {
    ptx_svi_Type                     :: !N1
    ,ptx_svi                         :: !N2
} deriving (Show, Typeable)

