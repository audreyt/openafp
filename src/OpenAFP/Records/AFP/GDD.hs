{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.GDD where
import OpenAFP.Types
import OpenAFP.Internals
 
data GDD = GDD {
    gdd_Type                         :: !N3
    ,gdd_                            :: !N3
    ,gdd                             :: !NStr
} deriving (Show, Typeable)

