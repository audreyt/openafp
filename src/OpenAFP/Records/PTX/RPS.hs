{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.PTX.RPS where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_RPS = PTX_RPS {
    ptx_rps_Type                     :: !N1
    ,ptx_rps                         :: !NStr
} deriving (Show, Typeable)

