module OpenAFP.Records.PTX.AMI where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_AMI = PTX_AMI {
    ptx_ami_Type                     :: !N1
    ,ptx_ami                         :: !N2
} deriving (Show, Typeable)

