{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.LLE where
import OpenAFP.Types
import OpenAFP.Internals
 
data LLE = LLE {
    lle_Type                         :: !N3
    ,lle_                            :: !N3
    ,lle                             :: !NStr
} deriving (Show, Typeable)

