{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EDM where
import OpenAFP.Types
import OpenAFP.Internals
 
data EDM = EDM {
    edm_Type                         :: !N3
    ,edm_                            :: !N3
    ,edm                             :: !NStr
} deriving (Show, Typeable)

