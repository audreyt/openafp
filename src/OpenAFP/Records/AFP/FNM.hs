{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.FNM where
import OpenAFP.Types
import OpenAFP.Internals

data FNM = FNM {
    fnm_Type                         :: !N3
    ,fnm_                            :: !N3
    ,fnm_Data                        :: ![Record FNM_Data]
} deriving (Show, Typeable)

data FNM_Data = FNM_Data {
     fnm_Width                       :: !N2
    ,fnm_Height                      :: !N2
    ,fnm_Offset                      :: !N4
} deriving (Show, Typeable)
 
