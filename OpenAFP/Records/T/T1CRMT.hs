{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.T1CRMT where
import OpenAFP.Types
import OpenAFP.Internals

data T_T1CRMT = T_T1CRMT {
    t_t1crmt_Type                    :: !N1
    ,t_t1crmt                        :: !NStr
} deriving (Show, Typeable)

