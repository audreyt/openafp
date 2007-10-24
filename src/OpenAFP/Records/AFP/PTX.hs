{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.PTX where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_ = PTX_ !N1 !Buffer1 deriving (Show, Typeable)

data PTX = PTX {
    ptx_Type                         :: !N3
    ,ptx_                            :: !N3
    ,ptx_EscapeSequence              :: !N2
    ,ptx_Chunks                      :: ![PTX_]
} deriving (Show, Typeable)

