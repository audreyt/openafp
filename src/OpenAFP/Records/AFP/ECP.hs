{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.ECP where
import OpenAFP.Types
import OpenAFP.Internals
 
data ECP = ECP {
    ecp_Type                         :: !N3
    ,ecp_                            :: !N3
    ,ecp                             :: !NStr
} deriving (Show, Typeable)

