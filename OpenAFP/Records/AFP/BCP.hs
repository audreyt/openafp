{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BCP where
import OpenAFP.Types
import OpenAFP.Internals
 
data BCP = BCP {
    bcp_Type                         :: !N3
    ,bcp_                            :: !N3
    ,bcp                             :: !NStr
} deriving (Show, Typeable)

