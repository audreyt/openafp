
module OpenAFP.Prelude.InstanceAFP.I () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

instance Rec ICP where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; return $ ICP a01 a02 a03 a04 a05 a06 a07 a08
    recPut r = do put $ icp_Type r; put $ icp_ r; put $ icp_XCellOffset r; put $ icp_YCellOffset r; put $ icp_XCellSize r; put $ icp_YCellSize r; put $ icp_XFillSize r; put $ icp_YFillSize r; return ()
    recSizeOf r = sum [ sizeOf $ icp_Type r, sizeOf $ icp_ r, sizeOf $ icp_XCellOffset r, sizeOf $ icp_YCellOffset r, sizeOf $ icp_XCellSize r, sizeOf $ icp_YCellSize r, sizeOf $ icp_XFillSize r, sizeOf $ icp_YFillSize r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ icp_Type r), viewField "_" (viewNumber $ icp_ r), viewField "XCellOffset" (viewNumber $ icp_XCellOffset r), viewField "YCellOffset" (viewNumber $ icp_YCellOffset r), viewField "XCellSize" (viewNumber $ icp_XCellSize r), viewField "YCellSize" (viewNumber $ icp_YCellSize r), viewField "XFillSize" (viewNumber $ icp_XFillSize r), viewField "YFillSize" (viewNumber $ icp_YFillSize r) ]
    recType = fromEnum . icp_Type

instance Rec IDD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IDD a01 a02 a03
    recPut r = do put $ idd_Type r; put $ idd_ r; put $ idd r; return ()
    recSizeOf r = sum [ sizeOf $ idd_Type r, sizeOf $ idd_ r, sizeOf $ idd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ idd_Type r), viewField "_" (viewNumber $ idd_ r), viewField "" (viewNStr $ idd r) ]
    recType = fromEnum . idd_Type

instance Rec IEL where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IEL a01 a02 a03
    recPut r = do put $ iel_Type r; put $ iel_ r; put $ iel r; return ()
    recSizeOf r = sum [ sizeOf $ iel_Type r, sizeOf $ iel_ r, sizeOf $ iel r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ iel_Type r), viewField "_" (viewNumber $ iel_ r), viewField "" (viewNStr $ iel r) ]
    recType = fromEnum . iel_Type

instance Rec IID where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; a09 <- get; a10 <- get; a11 <- get; a12 <- get; a13 <- get; a14 <- get; return $ IID a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14
    recPut r = do put $ iid_Type r; put $ iid_ r; put $ iid_ConstantData1 r; put $ iid_XBase r; put $ iid_YBase r; put $ iid_XUnits r; put $ iid_YUnits r; put $ iid_XSize r; put $ iid_YSize r; put $ iid_ConstantData2 r; put $ iid_XCellSizeDefault r; put $ iid_YCellSizeDefault r; put $ iid_ConstantData3 r; put $ iid_Color r; return ()
    recSizeOf r = sum [ sizeOf $ iid_Type r, sizeOf $ iid_ r, sizeOf $ iid_ConstantData1 r, sizeOf $ iid_XBase r, sizeOf $ iid_YBase r, sizeOf $ iid_XUnits r, sizeOf $ iid_YUnits r, sizeOf $ iid_XSize r, sizeOf $ iid_YSize r, sizeOf $ iid_ConstantData2 r, sizeOf $ iid_XCellSizeDefault r, sizeOf $ iid_YCellSizeDefault r, sizeOf $ iid_ConstantData3 r, sizeOf $ iid_Color r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ iid_Type r), viewField "_" (viewNumber $ iid_ r), viewField "ConstantData1" (viewString $ iid_ConstantData1 r), viewField "XBase" (viewNumber $ iid_XBase r), viewField "YBase" (viewNumber $ iid_YBase r), viewField "XUnits" (viewNumber $ iid_XUnits r), viewField "YUnits" (viewNumber $ iid_YUnits r), viewField "XSize" (viewNumber $ iid_XSize r), viewField "YSize" (viewNumber $ iid_YSize r), viewField "ConstantData2" (viewString $ iid_ConstantData2 r), viewField "XCellSizeDefault" (viewNumber $ iid_XCellSizeDefault r), viewField "YCellSizeDefault" (viewNumber $ iid_YCellSizeDefault r), viewField "ConstantData3" (viewString $ iid_ConstantData3 r), viewField "Color" (viewNumber $ iid_Color r) ]
    recType = fromEnum . iid_Type

instance Rec IMM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IMM a01 a02 a03
    recPut r = do put $ imm_Type r; put $ imm_ r; put $ imm r; return ()
    recSizeOf r = sum [ sizeOf $ imm_Type r, sizeOf $ imm_ r, sizeOf $ imm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ imm_Type r), viewField "_" (viewNumber $ imm_ r), viewField "" (viewNStr $ imm r) ]
    recType = fromEnum . imm_Type

instance Rec IOB where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IOB a01 a02 a03
    recPut r = do put $ iob_Type r; put $ iob_ r; put $ iob r; return ()
    recSizeOf r = sum [ sizeOf $ iob_Type r, sizeOf $ iob_ r, sizeOf $ iob r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ iob_Type r), viewField "_" (viewNumber $ iob_ r), viewField "" (viewNStr $ iob r) ]
    recType = fromEnum . iob_Type

instance Rec IOC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; a09 <- get; a10 <- get; a11 <- get; a12 <- get; return $ IOC a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12
    recPut r = do put $ ioc_Type r; put $ ioc_ r; put $ ioc_Reserved1 r; put $ ioc_XOffset r; put $ ioc_Reserved2 r; put $ ioc_YOffset r; put $ ioc_XOrientation r; put $ ioc_YOrientation r; put $ ioc_ConstantData1 r; put $ ioc_XMap r; put $ ioc_YMap r; put $ ioc_ConstantData2 r; return ()
    recSizeOf r = sum [ sizeOf $ ioc_Type r, sizeOf $ ioc_ r, sizeOf $ ioc_Reserved1 r, sizeOf $ ioc_XOffset r, sizeOf $ ioc_Reserved2 r, sizeOf $ ioc_YOffset r, sizeOf $ ioc_XOrientation r, sizeOf $ ioc_YOrientation r, sizeOf $ ioc_ConstantData1 r, sizeOf $ ioc_XMap r, sizeOf $ ioc_YMap r, sizeOf $ ioc_ConstantData2 r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ioc_Type r), viewField "_" (viewNumber $ ioc_ r), viewField "Reserved1" (viewString $ ioc_Reserved1 r), viewField "XOffset" (viewNumber $ ioc_XOffset r), viewField "Reserved2" (viewString $ ioc_Reserved2 r), viewField "YOffset" (viewNumber $ ioc_YOffset r), viewField "XOrientation" (viewNumber $ ioc_XOrientation r), viewField "YOrientation" (viewNumber $ ioc_YOrientation r), viewField "ConstantData1" (viewString $ ioc_ConstantData1 r), viewField "XMap" (viewNumber $ ioc_XMap r), viewField "YMap" (viewNumber $ ioc_YMap r), viewField "ConstantData2" (viewString $ ioc_ConstantData2 r) ]
    recType = fromEnum . ioc_Type

instance Rec IPD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IPD a01 a02 a03
    recPut r = do put $ ipd_Type r; put $ ipd_ r; put $ ipd r; return ()
    recSizeOf r = sum [ sizeOf $ ipd_Type r, sizeOf $ ipd_ r, sizeOf $ ipd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ipd_Type r), viewField "_" (viewNumber $ ipd_ r), viewField "" (viewNStr $ ipd r) ]
    recType = fromEnum . ipd_Type

instance Rec IPG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IPG a01 a02 a03
    recPut r = do put $ ipg_Type r; put $ ipg_ r; put $ ipg r; return ()
    recSizeOf r = sum [ sizeOf $ ipg_Type r, sizeOf $ ipg_ r, sizeOf $ ipg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ipg_Type r), viewField "_" (viewNumber $ ipg_ r), viewField "" (viewNStr $ ipg r) ]
    recType = fromEnum . ipg_Type

instance Rec IPO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IPO a01 a02 a03
    recPut r = do put $ ipo_Type r; put $ ipo_ r; put $ ipo r; return ()
    recSizeOf r = sum [ sizeOf $ ipo_Type r, sizeOf $ ipo_ r, sizeOf $ ipo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ipo_Type r), viewField "_" (viewNumber $ ipo_ r), viewField "" (viewNStr $ ipo r) ]
    recType = fromEnum . ipo_Type

instance Rec IPS where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IPS a01 a02 a03
    recPut r = do put $ ips_Type r; put $ ips_ r; put $ ips r; return ()
    recSizeOf r = sum [ sizeOf $ ips_Type r, sizeOf $ ips_ r, sizeOf $ ips r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ips_Type r), viewField "_" (viewNumber $ ips_ r), viewField "" (viewAStr $ ips r) ]
    recType = fromEnum . ips_Type

instance Rec IRD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IRD a01 a02 a03
    recPut r = do put $ ird_Type r; put $ ird_ r; put $ ird_ImageData r; return ()
    recSizeOf r = sum [ sizeOf $ ird_Type r, sizeOf $ ird_ r, sizeOf $ ird_ImageData r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ird_Type r), viewField "_" (viewNumber $ ird_ r), viewField "ImageData" (viewNStr $ ird_ImageData r) ]
    recType = fromEnum . ird_Type

