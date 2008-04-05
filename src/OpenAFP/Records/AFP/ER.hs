{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.ER where
import OpenAFP.Types
import OpenAFP.Internals
 
data ER = ER {
    er_Type                          :: !N3
    ,er_                             :: !N3
    ,er                              :: !AStr
} deriving (Show, Typeable)

