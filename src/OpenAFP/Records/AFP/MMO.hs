{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.MMO where
import OpenAFP.Types
import OpenAFP.Internals
 
data MMO = MMO {
    mmo_Type                         :: !N3
    ,mmo_                            :: !N3
    ,mmo                             :: !NStr
} deriving (Show, Typeable)

