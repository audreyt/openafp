
module OpenAFP.Records.AFP.ICP where
import OpenAFP.Types
import OpenAFP.Internals
 
data ICP = ICP {
    icp_Type                         :: !N3
    ,icp_                            :: !N3
    ,icp_XCellOffset                 :: !N2
    ,icp_YCellOffset                 :: !N2
    ,icp_XCellSize                   :: !N2
    ,icp_YCellSize                   :: !N2
    ,icp_XFillSize                   :: !N2
    ,icp_YFillSize                   :: !N2
} deriving (Show, Typeable)

