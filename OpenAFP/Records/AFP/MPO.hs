{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.MPO where
import OpenAFP.Types
import OpenAFP.Internals
 
data MPO = MPO {
    mpo_Type                         :: !N3
    ,mpo_                            :: !N3
    ,mpo                             :: !NStr
} deriving (Show, Typeable)

