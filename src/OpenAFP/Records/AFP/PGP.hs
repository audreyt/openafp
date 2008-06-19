
module OpenAFP.Records.AFP.PGP where
import OpenAFP.Types
import OpenAFP.Internals
 
data PGP = PGP {
    pgp_Type                         :: !N3
    ,pgp_                            :: !N3
    ,pgp                             :: !NStr
} deriving (Show, Typeable)

