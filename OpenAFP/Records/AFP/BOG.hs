{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BOG where
import OpenAFP.Types
import OpenAFP.Internals
 
data BOG = BOG {
    bog_Type                         :: !N3
    ,bog_                            :: !N3
    ,bog                             :: !NStr
} deriving (Show, Typeable)

