{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.ENG where
import OpenAFP.Types
import OpenAFP.Internals
 
data ENG = ENG {
    eng_Type                         :: !N3
    ,eng_                            :: !N3
    ,eng                             :: !NStr
} deriving (Show, Typeable)

