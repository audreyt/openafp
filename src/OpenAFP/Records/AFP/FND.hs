{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.FND where
import OpenAFP.Types
import OpenAFP.Internals
 
data FND = FND {
    fnd_Type                         :: !N3
    ,fnd_                            :: !N3
    ,fnd                             :: !NStr
} deriving (Show, Typeable)

