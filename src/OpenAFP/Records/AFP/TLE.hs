{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.TLE where
import OpenAFP.Types
import OpenAFP.Internals

data TLE = TLE {
    tle_Type                         :: !N3
    ,tle_                            :: !N3
    ,tle_Chunks                      :: ![T_]
    ,tle_XUnitBase                   :: !N1
    ,tle_YUnitBase                   :: !N1
    ,tle_XLUnitsperUnitBase          :: !N2
    ,tle_YLUnitsperUnitBase          :: !N2
    ,tle_XPageSize                   :: !N3
    ,tle_YPageSize                   :: !N3
    ,tle__                           :: !NStr
} deriving (Show, Typeable)

