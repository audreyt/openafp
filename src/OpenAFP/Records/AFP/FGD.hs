{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.FGD where
import OpenAFP.Types
import OpenAFP.Internals
 
data FGD = FGD {
    fgd_Type                         :: !N3
    ,fgd_                            :: !N3
    ,fgd                             :: !NStr
} deriving (Show, Typeable)

