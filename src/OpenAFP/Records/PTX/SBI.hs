module OpenAFP.Records.PTX.SBI where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_SBI = PTX_SBI {
    ptx_sbi_Type                     :: !N1
    ,ptx_sbi                         :: !N2
} deriving (Show, Typeable)

