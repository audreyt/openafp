{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.FRMT where
import OpenAFP.Types
import OpenAFP.Internals

data T_FRMT = T_FRMT {
    t_frmt_Type                      :: !N1
    ,t_frmt                          :: !NStr
} deriving (Show, Typeable)

