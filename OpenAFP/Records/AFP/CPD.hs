{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.CPD where
import OpenAFP.Types
import OpenAFP.Internals
 
data CPD = CPD {
    cpd_Type                         :: !N3
    ,cpd_                            :: !N3
    ,cpd                             :: !NStr
} deriving (Show, Typeable)

