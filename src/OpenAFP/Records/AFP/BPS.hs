{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BPS where
import OpenAFP.Types
import OpenAFP.Internals
 
data BPS = BPS {
    bps_Type                         :: !N3
    ,bps_                            :: !N3
    ,bps                             :: !NStr
} deriving (Show, Typeable)

