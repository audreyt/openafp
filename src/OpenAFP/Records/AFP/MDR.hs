{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.MDR where
import OpenAFP.Types
import OpenAFP.Internals
 
data MDR = MDR {
    mdr_Type                         :: !N3
    ,mdr_                            :: !N3
    ,mdr                             :: !NStr
} deriving (Show, Typeable)

