{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.CR where
import OpenAFP.Types
import OpenAFP.Internals

data T_CR = T_CR {
    t_cr_Type                        :: !N1
    ,t_cr                            :: !NStr
} deriving (Show, Typeable)

