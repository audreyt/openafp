{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.IEL where
import OpenAFP.Types
import OpenAFP.Internals
 
data IEL = IEL {
    iel_Type                         :: !N3
    ,iel_                            :: !N3
    ,iel                             :: !NStr
} deriving (Show, Typeable)

