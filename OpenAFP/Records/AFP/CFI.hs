{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.CFI where
import OpenAFP.Types
import OpenAFP.Internals

data CFI = CFI {
    cfi_Type                         :: !N3
    ,cfi_                            :: !N3
    ,cfi_Data                        :: ![Record CFI_Data]
} deriving (Show, Typeable)

data CFI_Data = CFI_Data {
     cfi_FontCharacterSetName        :: !A8
    ,cfi_CodePageName                :: !A8
    ,cfi_CodedFontName               :: !A8
    ,cfi_Section                     :: !N1
} deriving (Show, Typeable)

