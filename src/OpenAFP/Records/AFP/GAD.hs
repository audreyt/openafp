{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.GAD where
import OpenAFP.Types
import OpenAFP.Internals
 
data GAD = GAD {
    gad_Type                         :: !N3
    ,gad_                            :: !N3
    ,gad                             :: !NStr
} deriving (Show, Typeable)

