{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BSG where
import OpenAFP.Types
import OpenAFP.Internals
 
data BSG = BSG {
    bsg_Type                         :: !N3
    ,bsg_                            :: !N3
    ,bsg                             :: !NStr
} deriving (Show, Typeable)

