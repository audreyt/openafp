{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EOC where
import OpenAFP.Types
import OpenAFP.Internals
 
data EOC = EOC {
    eoc_Type                         :: !N3
    ,eoc_                            :: !N3
    ,eoc                             :: !NStr
} deriving (Show, Typeable)

