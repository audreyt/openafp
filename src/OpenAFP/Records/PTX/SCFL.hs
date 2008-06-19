module OpenAFP.Records.PTX.SCFL where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_SCFL = PTX_SCFL {
    ptx_scfl_Type                    :: !N1
    ,ptx_scfl                        :: !N1
} deriving (Show, Typeable)

