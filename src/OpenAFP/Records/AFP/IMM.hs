
module OpenAFP.Records.AFP.IMM where
import OpenAFP.Types
import OpenAFP.Internals
 
data IMM = IMM {
    imm_Type                         :: !N3
    ,imm_                            :: !N3
    ,imm                             :: !NStr
} deriving (Show, Typeable)

