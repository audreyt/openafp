{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.MMC where
import OpenAFP.Types
import OpenAFP.Internals
 
data MMC = MMC {
    mmc_Type                         :: !N3
    ,mmc_                            :: !N3
    ,mmc                             :: !NStr
} deriving (Show, Typeable)

