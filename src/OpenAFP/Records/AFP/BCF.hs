{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BCF where
import OpenAFP.Types
import OpenAFP.Internals
 
data BCF = BCF {
    bcf_Type                         :: !N3
    ,bcf_                            :: !N3
    ,bcf                             :: !NStr
} deriving (Show, Typeable)

