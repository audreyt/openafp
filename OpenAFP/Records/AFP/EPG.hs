{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EPG where
import OpenAFP.Types
import OpenAFP.Internals
 
data EPG = EPG {
    epg_Type                         :: !N3
    ,epg_                            :: !N3
    ,epg                             :: !NStr
} deriving (Show, Typeable)

