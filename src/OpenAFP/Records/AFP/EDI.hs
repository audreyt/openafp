{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.EDI where
import OpenAFP.Types
import OpenAFP.Internals
 
data EDI = EDI {
    edi_Type                         :: !N3
    ,edi_                            :: !N3
    ,edi                             :: !NStr
} deriving (Show, Typeable)

