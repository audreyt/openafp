{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EPS where
import OpenAFP.Types
import OpenAFP.Internals
 
data EPS = EPS {
    eps_Type                         :: !N3
    ,eps_                            :: !N3
    ,eps                             :: !NStr
} deriving (Show, Typeable)

