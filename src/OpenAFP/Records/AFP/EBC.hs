{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EBC where
import OpenAFP.Types
import OpenAFP.Internals
 
data EBC = EBC {
    ebc_Type                         :: !N3
    ,ebc_                            :: !N3
    ,ebc                             :: !NStr
} deriving (Show, Typeable)

