{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EFG where
import OpenAFP.Types
import OpenAFP.Internals
 
data EFG = EFG {
    efg_Type                         :: !N3
    ,efg_                            :: !N3
    ,efg                             :: !NStr
} deriving (Show, Typeable)

