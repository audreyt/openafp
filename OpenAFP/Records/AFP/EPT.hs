{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EPT where
import OpenAFP.Types
import OpenAFP.Internals
 
data EPT = EPT {
    ept_Type                         :: !N3
    ,ept_                            :: !N3
    ,ept                             :: !NStr
} deriving (Show, Typeable)

