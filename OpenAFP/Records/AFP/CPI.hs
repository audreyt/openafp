{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.CPI where
import OpenAFP.Types
import OpenAFP.Internals

data CPI = CPI {
    cpi_Type                         :: !N3
    ,cpi_                            :: !N3
    ,cpi_Data                        :: ![Record CPI_Data]
} deriving (Show, Typeable)

data CPI_Data = CPI_Data {
     cpi_GCGID                       :: !A8
    ,cpi_Section                     :: !N1
    ,cpi_CodePoint                   :: !N1
} deriving (Show, Typeable)
 
