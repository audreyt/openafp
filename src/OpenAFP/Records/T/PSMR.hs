{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.PSMR where
import OpenAFP.Types
import OpenAFP.Internals

data T_PSMR = T_PSMR {
    t_psmr_Type                      :: !N1
    ,t_psmr                          :: !NStr
} deriving (Show, Typeable)

