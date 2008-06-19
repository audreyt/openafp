module OpenAFP.Records.PTX.BLN where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_BLN = PTX_BLN {
    ptx_bln_Type                     :: !N1
    ,ptx_bln                         :: !NStr
} deriving (Show, Typeable)

