{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.PGD where
import OpenAFP.Types
import OpenAFP.Internals
 
data PGD = PGD {
    pgd_Type                         :: !N3
    ,pgd_                            :: !N3
    ,pgd_XUnitBase                   :: !N1
    ,pgd_YUnitBase                   :: !N1
    ,pgd_XLUnitsperUnitBase          :: !N2
    ,pgd_YLUnitsperUnitBase          :: !N2
    ,pgd_Reserved1                   :: !N1
    ,pgd_XPageSize                   :: !N2
    ,pgd_Reserved2                   :: !N1
    ,pgd_YPageSize                   :: !N2
    ,pgd__                           :: !NStr
} deriving (Show, Typeable)

