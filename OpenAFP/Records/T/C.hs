{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.C where
import OpenAFP.Types
import OpenAFP.Internals

data T_C = T_C {
    t_c_Type                         :: !N1
    ,t_c                             :: !NStr
} deriving (Show, Typeable)

