{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EGR where
import OpenAFP.Types
import OpenAFP.Internals
 
data EGR = EGR {
    egr_Type                         :: !N3
    ,egr_                            :: !N3
    ,egr                             :: !NStr
} deriving (Show, Typeable)

