{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BDX where
import OpenAFP.Types
import OpenAFP.Internals
 
data BDX = BDX {
    bdx_Type                         :: !N3
    ,bdx_                            :: !N3
    ,bdx                             :: !NStr
} deriving (Show, Typeable)

