{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EAG where
import OpenAFP.Types
import OpenAFP.Internals
 
data EAG = EAG {
    eag_Type                         :: !N3
    ,eag_                            :: !N3
    ,eag                             :: !NStr
} deriving (Show, Typeable)

