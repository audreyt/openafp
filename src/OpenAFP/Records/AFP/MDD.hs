{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.MDD where
import OpenAFP.Types
import OpenAFP.Internals
 
data MDD = MDD {
    mdd_Type                         :: !N3
    ,mdd_                            :: !N3
    ,mdd                             :: !NStr
} deriving (Show, Typeable)

