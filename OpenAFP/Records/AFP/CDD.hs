{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.CDD where
import OpenAFP.Types
import OpenAFP.Internals
 
data CDD = CDD {
    cdd_Type                         :: !N3
    ,cdd_                            :: !N3
    ,cdd                             :: !NStr
} deriving (Show, Typeable)

