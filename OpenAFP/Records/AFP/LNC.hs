{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.LNC where
import OpenAFP.Types
import OpenAFP.Internals
 
data LNC = LNC {
    lnc_Type                         :: !N3
    ,lnc_                            :: !N3
    ,lnc                             :: !NStr
} deriving (Show, Typeable)

