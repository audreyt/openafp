{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.PTX.ESU where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_ESU = PTX_ESU {
    ptx_esu_Type                     :: !N1
    ,ptx_esu                         :: !NStr
} deriving (Show, Typeable)

