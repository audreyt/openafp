{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.PTX.BSU where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_BSU = PTX_BSU {
    ptx_bsu_Type                     :: !N1
    ,ptx_bsu                         :: !NStr
} deriving (Show, Typeable)

