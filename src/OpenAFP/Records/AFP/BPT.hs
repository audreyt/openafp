{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BPT where
import OpenAFP.Types
import OpenAFP.Internals
 
data BPT = BPT {
    bpt_Type                         :: !N3
    ,bpt_                            :: !N3
    ,bpt                             :: !NStr
} deriving (Show, Typeable)

