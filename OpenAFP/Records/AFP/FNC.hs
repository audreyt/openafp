{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.FNC where
import OpenAFP.Types
import OpenAFP.Internals

--- XXX - not parsed completely - see FNC.pm
 
data FNC = FNC {
    fnc_Type                         :: !N3
    ,fnc_                            :: !N3
    ,fnc_Constant                    :: !A1
    ,fnc_PatternTechnologyIdentifier :: !N1
    ,fnc_FNCReserved1                :: !A1
    ,fnc_UseFlags                    :: !N1
    ,fnc_UnitXBase                   :: !N1
    ,fnc_UnitYBase                   :: !N1
    ,fnc_UnitXValue                  :: !N2
    ,fnc_UnitYValue                  :: !N2
    ,fnc_MaxWidth                    :: !N2
    ,fnc_MaxHeight                   :: !N2
    ,fnc_FNORepeatingGroupLength     :: !N1
    ,fnc_FNIRepeatingGroupLength     :: !N1
    ,fnc_PatternDataAlignmentCode    :: !N1
    ,fnc_PatternDataCount1           :: !A3
    ,fnc_FNPRepeatingGroupLength     :: !N1
    ,fnc_FNMRepeatingGroupLength     :: !N1
    ,fnc                             :: !NStr
} deriving (Show, Typeable)

