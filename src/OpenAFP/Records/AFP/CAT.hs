{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.CAT where
import OpenAFP.Types
import OpenAFP.Internals
 
data CAT = CAT {
    cat_Type                         :: !N3
    ,cat_                            :: !N3
    ,cat                             :: !NStr
} deriving (Show, Typeable)

