{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.OBP where
import OpenAFP.Types
import OpenAFP.Internals
 
data OBP = OBP {
    obp_Type                         :: !N3
    ,obp_                            :: !N3
    ,obp                             :: !NStr
} deriving (Show, Typeable)

