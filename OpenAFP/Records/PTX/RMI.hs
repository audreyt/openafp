{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.PTX.RMI where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_RMI = PTX_RMI {
    ptx_rmi_Type                     :: !N1
    ,ptx_rmi                         :: !N2
} deriving (Show, Typeable)

