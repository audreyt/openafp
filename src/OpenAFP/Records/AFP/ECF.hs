{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.ECF where
import OpenAFP.Types
import OpenAFP.Internals
 
data ECF = ECF {
    ecf_Type                         :: !N3
    ,ecf_                            :: !N3
    ,ecf                             :: !NStr
} deriving (Show, Typeable)

