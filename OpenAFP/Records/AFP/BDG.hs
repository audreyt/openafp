{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BDG where
import OpenAFP.Types
import OpenAFP.Internals
 
data BDG = BDG {
    bdg_Type                         :: !N3
    ,bdg_                            :: !N3
    ,bdg                             :: !NStr
} deriving (Show, Typeable)

