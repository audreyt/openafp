{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.PFC where
import OpenAFP.Types
import OpenAFP.Internals
 
data PFC = PFC {
    pfc_Type                         :: !N3
    ,pfc_                            :: !N3
    ,pfc                             :: !NStr
} deriving (Show, Typeable)

