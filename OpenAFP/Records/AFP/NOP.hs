{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.NOP where
import OpenAFP.Types
import OpenAFP.Internals
 
data NOP = NOP {
    nop_Type                         :: !N3
    ,nop_                            :: !N3
    ,nop                             :: !NStr
} deriving (Show, Typeable)

