{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.ESG where
import OpenAFP.Types
import OpenAFP.Internals
 
data ESG = ESG {
    esg_Type                         :: !N3
    ,esg_                            :: !N3
    ,esg                             :: !NStr
} deriving (Show, Typeable)

