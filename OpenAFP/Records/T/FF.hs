{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.FF where
import OpenAFP.Types
import OpenAFP.Internals

data T_FF = T_FF {
    t_ff_Type                        :: !N1
    ,t_ff                            :: !NStr
} deriving (Show, Typeable)

