module OpenAFP.Records.PTX.SIM where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_SIM = PTX_SIM {
    ptx_sim_Type                     :: !N1
    ,ptx_sim                         :: !N2
} deriving (Show, Typeable)

