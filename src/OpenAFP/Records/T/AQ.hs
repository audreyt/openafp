{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.AQ where
import OpenAFP.Types
import OpenAFP.Internals

data T_AQ = T_AQ {
    t_aq_Type                        :: !N1
    ,t_aq                            :: !NStr
} deriving (Show, Typeable)

