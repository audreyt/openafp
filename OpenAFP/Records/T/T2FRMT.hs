{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.T2FRMT where
import OpenAFP.Types
import OpenAFP.Internals

data T_T2FRMT = T_T2FRMT {
    t_t2frmt_Type                    :: !N1
    ,t_t2frmt                        :: !NStr
} deriving (Show, Typeable)

