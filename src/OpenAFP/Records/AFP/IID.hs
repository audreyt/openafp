{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

module OpenAFP.Records.AFP.IID where
import OpenAFP.Types
import OpenAFP.Internals
 
data IID = IID {
    iid_Type                         :: !N3
    ,iid_                            :: !N3
    ,iid_ConstantData1               :: !A12
    ,iid_XBase                       :: !N1
    ,iid_YBase                       :: !N1
    ,iid_XUnits                      :: !N2
    ,iid_YUnits                      :: !N2
    ,iid_XSize                       :: !N2
    ,iid_YSize                       :: !N2
    ,iid_ConstantData2               :: !A6
    ,iid_XCellSizeDefault            :: !N2
    ,iid_YCellSizeDefault            :: !N2
    ,iid_ConstantData3               :: !A1
    ,iid_Color                       :: !N2
} deriving (Show, Typeable)

