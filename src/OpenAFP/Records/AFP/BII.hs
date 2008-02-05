{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BII where
import OpenAFP.Types
import OpenAFP.Internals
 
data BII = BII {
    bii_Type                         :: !N3
    ,bii_                            :: !N3
    ,bii_ImageObjectName             :: !AStr
} deriving (Show, Typeable)

