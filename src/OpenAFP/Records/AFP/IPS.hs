{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.IPS where
import OpenAFP.Types
import OpenAFP.Internals
 
data IPS = IPS {
    ips_Type                         :: !N3
    ,ips_                            :: !N3
    ,ips                             :: !NStr
} deriving (Show, Typeable)

