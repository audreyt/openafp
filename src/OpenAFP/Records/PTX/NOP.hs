module OpenAFP.Records.PTX.NOP where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_NOP = PTX_NOP {
    ptx_nop_Type                     :: !N1
    ,ptx_nop                         :: !NStr
} deriving (Show, Typeable)

