{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.BGR where
import OpenAFP.Types
import OpenAFP.Internals
 
data BGR = BGR {
    bgr_Type                         :: !N3
    ,bgr_                            :: !N3
    ,bgr                             :: !NStr
} deriving (Show, Typeable)

