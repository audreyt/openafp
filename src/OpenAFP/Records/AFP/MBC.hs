{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.MBC where
import OpenAFP.Types
import OpenAFP.Internals
 
data MBC = MBC {
    mbc_Type                         :: !N3
    ,mbc_                            :: !N3
    ,mbc                             :: !NStr
} deriving (Show, Typeable)

