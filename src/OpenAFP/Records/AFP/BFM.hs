{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BFM where
import OpenAFP.Types
import OpenAFP.Internals
 
data BFM = BFM {
    bfm_Type                         :: !N3
    ,bfm_                            :: !N3
    ,bfm                             :: !NStr
} deriving (Show, Typeable)

