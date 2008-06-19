module OpenAFP.Records.PTX.DIR where
import OpenAFP.Types
import OpenAFP.Internals

data PTX_DIR = PTX_DIR {
    ptx_dir_Type                     :: !N1
    ,ptx_dir                         :: !NStr
} deriving (Show, Typeable)

