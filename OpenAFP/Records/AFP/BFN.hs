{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BFN where
import OpenAFP.Types
import OpenAFP.Internals
 
data BFN = BFN {
    bfn_Type                         :: !N3
    ,bfn_                            :: !N3
    ,bfn                             :: !NStr
} deriving (Show, Typeable)

