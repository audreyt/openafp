{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.MCF1 where
import OpenAFP.Types
import OpenAFP.Internals

data MCF1_Data = MCF1_Data {
     mcf1_CodedFontLocalId             :: !N1
    ,mcf1_Reserved1                    :: !A1
    ,mcf1_CodedFontResourceSectionId   :: !N1
    ,mcf1_Reserved2                    :: !A1
    ,mcf1_CodedFontName                :: !A8
    ,mcf1_CodePageName                 :: !A8
    ,mcf1_FontCharacterSetName         :: !A8
    ,mcf1_CharacterRotation            :: !N2
} deriving (Show, Typeable)

data MCF1 = MCF1 {
    mcf1_Type                        :: !N3
    ,mcf1_                           :: !N3
    ,mcf1_RepeatingGroupLength       :: !N1
    ,mcf1__                          :: !A3
    ,mcf1_Data                       :: ![Record MCF1_Data]
} deriving (Show, Typeable)

