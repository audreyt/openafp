{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.PTX.DBR where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_DBR = PTX_DBR {
    ptx_dbr_Type                     :: !N1
    ,ptx_dbr                         :: !NStr
} deriving (Show, Typeable)

