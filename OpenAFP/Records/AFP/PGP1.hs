{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.PGP1 where
import OpenAFP.Types
import OpenAFP.Internals
 
data PGP1 = PGP1 {
    pgp1_Type                        :: !N3
    ,pgp1_                           :: !N3
    ,pgp1                            :: !NStr
} deriving (Show, Typeable)

