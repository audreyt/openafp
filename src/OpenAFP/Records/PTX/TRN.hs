module OpenAFP.Records.PTX.TRN where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_TRN = PTX_TRN {
    ptx_trn_Type                     :: !N1
    ,ptx_trn                         :: !NStr
} deriving (Show, Typeable)

