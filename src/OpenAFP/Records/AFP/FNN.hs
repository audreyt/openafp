{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.FNN where
import OpenAFP.Types
import OpenAFP.Internals
 
data FNN = FNN {
    fnn_Type                         :: !N3
    ,fnn_                            :: !N3
    ,fnn                             :: !NStr
} deriving (Show, Typeable)

