{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.PTX.SIA where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_SIA = PTX_SIA {
    ptx_sia_Type                     :: !N1
    ,ptx_sia                         :: !N2
} deriving (Show, Typeable)

