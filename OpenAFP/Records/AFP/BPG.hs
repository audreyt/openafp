{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BPG where
import OpenAFP.Types
import OpenAFP.Internals
 
data BPG = BPG {
    bpg_Type                         :: !N3
    ,bpg_                            :: !N3
    ,bpg                             :: !NStr
} deriving (Show, Typeable)

