{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BDM where
import OpenAFP.Types
import OpenAFP.Internals
 
data BDM = BDM {
    bdm_Type                         :: !N3
    ,bdm_                            :: !N3
    ,bdm                             :: !NStr
} deriving (Show, Typeable)

