{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.FHSF where
import OpenAFP.Types
import OpenAFP.Internals

data T_FHSF = T_FHSF {
    t_fhsf_Type                      :: !N1
    ,t_fhsf                          :: !NStr
} deriving (Show, Typeable)

