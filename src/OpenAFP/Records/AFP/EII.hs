{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EII where
import OpenAFP.Types
import OpenAFP.Internals
 
data EII = EII {
    eii_Type                         :: !N3
    ,eii_                            :: !N3
    ,eii_ImageObjectName             :: !A8
} deriving (Show, Typeable)

