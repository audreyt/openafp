
module OpenAFP.Records.AFP.IOC where
import OpenAFP.Types
import OpenAFP.Internals
 
data IOC = IOC {
    ioc_Type                         :: !N3
    ,ioc_                            :: !N3
    ,ioc_Reserved1                   :: !A1
    ,ioc_XOffset                     :: !N2
    ,ioc_Reserved2                   :: !A1
    ,ioc_YOffset                     :: !N2
    ,ioc_XOrientation                :: !N2
    ,ioc_YOrientation                :: !N2
    ,ioc_ConstantData1               :: !A8
    ,ioc_XMap                        :: !N2
    ,ioc_YMap                        :: !N2
    ,ioc_ConstantData2               :: !A2
} deriving (Show, Typeable)

