{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.PTX.STO where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_STO = PTX_STO {
    ptx_sto_Type                     :: !N1
    ,ptx_sto_Orientation             :: !N2
    ,ptx_sto_WrapDirection           :: !N2
} deriving (Show, Typeable)

