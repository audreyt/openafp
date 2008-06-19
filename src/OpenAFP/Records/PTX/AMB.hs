module OpenAFP.Records.PTX.AMB where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_AMB = PTX_AMB {
    ptx_amb_Type                     :: !N1
    ,ptx_amb                         :: !N2
} deriving (Show, Typeable)

