{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.MCA where
import OpenAFP.Types
import OpenAFP.Internals
 
data MCA = MCA {
    mca_Type                         :: !N3
    ,mca_                            :: !N3
    ,mca                             :: !NStr
} deriving (Show, Typeable)

