{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BDD where
import OpenAFP.Types
import OpenAFP.Internals
 
data BDD = BDD {
    bdd_Type                         :: !N3
    ,bdd_                            :: !N3
    ,bdd                             :: !NStr
} deriving (Show, Typeable)

