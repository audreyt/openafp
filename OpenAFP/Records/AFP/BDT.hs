{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BDT where
import OpenAFP.Types
import OpenAFP.Internals
 
data BDT = BDT {
    bdt_Type                         :: !N3
    ,bdt_                            :: !N3
    ,bdt                             :: !NStr
} deriving (Show, Typeable)

