{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.DP where
import OpenAFP.Types
import OpenAFP.Internals

data T_DP = T_DP {
    t_dp_Type                        :: !N1
    ,t_dp                            :: !NStr
} deriving (Show, Typeable)

