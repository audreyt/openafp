{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EFN where
import OpenAFP.Types
import OpenAFP.Internals
 
data EFN = EFN {
    efn_Type                         :: !N3
    ,efn_                            :: !N3
    ,efn                             :: !NStr
} deriving (Show, Typeable)

