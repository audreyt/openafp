
module OpenAFP.Records.AFP.DXD where
import OpenAFP.Types
import OpenAFP.Internals
 
data DXD = DXD {
    dxd_Type                         :: !N3
    ,dxd_                            :: !N3
    ,dxd                             :: !NStr
} deriving (Show, Typeable)

