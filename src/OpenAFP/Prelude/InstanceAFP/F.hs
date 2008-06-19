
module OpenAFP.Prelude.InstanceAFP.F () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

instance Rec FGD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ FGD a01 a02 a03
    recPut r = do put $ fgd_Type r; put $ fgd_ r; put $ fgd r; return ()
    recSizeOf r = sum [ sizeOf $ fgd_Type r, sizeOf $ fgd_ r, sizeOf $ fgd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fgd_Type r), viewField "_" (viewNumber $ fgd_ r), viewField "" (viewNStr $ fgd r) ]
    recType = fromEnum . fgd_Type

instance Rec FNC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; a09 <- get; a10 <- get; a11 <- get; a12 <- get; a13 <- get; a14 <- get; a15 <- get; a16 <- get; a17 <- get; a18 <- get; a19 <- get; return $ FNC a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19
    recPut r = do put $ fnc_Type r; put $ fnc_ r; put $ fnc_Constant r; put $ fnc_PatternTechnologyIdentifier r; put $ fnc_FNCReserved1 r; put $ fnc_UseFlags r; put $ fnc_UnitXBase r; put $ fnc_UnitYBase r; put $ fnc_UnitXValue r; put $ fnc_UnitYValue r; put $ fnc_MaxWidth r; put $ fnc_MaxHeight r; put $ fnc_FNORepeatingGroupLength r; put $ fnc_FNIRepeatingGroupLength r; put $ fnc_PatternDataAlignmentCode r; put $ fnc_PatternDataCount1 r; put $ fnc_FNPRepeatingGroupLength r; put $ fnc_FNMRepeatingGroupLength r; put $ fnc r; return ()
    recSizeOf r = sum [ sizeOf $ fnc_Type r, sizeOf $ fnc_ r, sizeOf $ fnc_Constant r, sizeOf $ fnc_PatternTechnologyIdentifier r, sizeOf $ fnc_FNCReserved1 r, sizeOf $ fnc_UseFlags r, sizeOf $ fnc_UnitXBase r, sizeOf $ fnc_UnitYBase r, sizeOf $ fnc_UnitXValue r, sizeOf $ fnc_UnitYValue r, sizeOf $ fnc_MaxWidth r, sizeOf $ fnc_MaxHeight r, sizeOf $ fnc_FNORepeatingGroupLength r, sizeOf $ fnc_FNIRepeatingGroupLength r, sizeOf $ fnc_PatternDataAlignmentCode r, sizeOf $ fnc_PatternDataCount1 r, sizeOf $ fnc_FNPRepeatingGroupLength r, sizeOf $ fnc_FNMRepeatingGroupLength r, sizeOf $ fnc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fnc_Type r), viewField "_" (viewNumber $ fnc_ r), viewField "Constant" (viewString $ fnc_Constant r), viewField "PatternTechnologyIdentifier" (viewNumber $ fnc_PatternTechnologyIdentifier r), viewField "FNCReserved1" (viewString $ fnc_FNCReserved1 r), viewField "UseFlags" (viewNumber $ fnc_UseFlags r), viewField "UnitXBase" (viewNumber $ fnc_UnitXBase r), viewField "UnitYBase" (viewNumber $ fnc_UnitYBase r), viewField "UnitXValue" (viewNumber $ fnc_UnitXValue r), viewField "UnitYValue" (viewNumber $ fnc_UnitYValue r), viewField "MaxWidth" (viewNumber $ fnc_MaxWidth r), viewField "MaxHeight" (viewNumber $ fnc_MaxHeight r), viewField "FNORepeatingGroupLength" (viewNumber $ fnc_FNORepeatingGroupLength r), viewField "FNIRepeatingGroupLength" (viewNumber $ fnc_FNIRepeatingGroupLength r), viewField "PatternDataAlignmentCode" (viewNumber $ fnc_PatternDataAlignmentCode r), viewField "PatternDataCount1" (viewString $ fnc_PatternDataCount1 r), viewField "FNPRepeatingGroupLength" (viewNumber $ fnc_FNPRepeatingGroupLength r), viewField "FNMRepeatingGroupLength" (viewNumber $ fnc_FNMRepeatingGroupLength r), viewField "" (viewNStr $ fnc r) ]
    recType = fromEnum . fnc_Type

instance Rec FND where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ FND a01 a02 a03
    recPut r = do put $ fnd_Type r; put $ fnd_ r; put $ fnd r; return ()
    recSizeOf r = sum [ sizeOf $ fnd_Type r, sizeOf $ fnd_ r, sizeOf $ fnd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fnd_Type r), viewField "_" (viewNumber $ fnd_ r), viewField "" (viewNStr $ fnd r) ]
    recType = fromEnum . fnd_Type

instance Rec FNG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ FNG a01 a02 a03
    recPut r = do put $ fng_Type r; put $ fng_ r; put $ fng r; return ()
    recSizeOf r = sum [ sizeOf $ fng_Type r, sizeOf $ fng_ r, sizeOf $ fng r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fng_Type r), viewField "_" (viewNumber $ fng_ r), viewField "" (viewNStr $ fng r) ]
    recType = fromEnum . fng_Type

instance Rec FNI where
    recGet = do a01 <- get; a02 <- get; a03 <- getList; return $ FNI a01 a02 a03
    recPut r = do put $ fni_Type r; put $ fni_ r; putList $ fni_Data r; return ()
    recSizeOf r = sum [ sizeOf $ fni_Type r, sizeOf $ fni_ r, sizeOf $ fni_Data r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fni_Type r), viewField "_" (viewNumber $ fni_ r), viewField "Data" (viewData $ fni_Data r) ]
    recType = fromEnum . fni_Type

instance Rec FNI_Data where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; a09 <- get; a10 <- get; a11 <- get; return $ FNI_Data a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11
    recPut r = do put $ fni_GCGID r; put $ fni_CharacterIncrement r; put $ fni_AscendHeight r; put $ fni_DescendDepth r; put $ fni_Reserved1 r; put $ fni_FNMCount r; put $ fni_ASpace r; put $ fni_BSpace r; put $ fni_CSpace r; put $ fni_Reserved2 r; put $ fni_BaseOffset r; return ()
    recSizeOf r = sum [ sizeOf $ fni_GCGID r, sizeOf $ fni_CharacterIncrement r, sizeOf $ fni_AscendHeight r, sizeOf $ fni_DescendDepth r, sizeOf $ fni_Reserved1 r, sizeOf $ fni_FNMCount r, sizeOf $ fni_ASpace r, sizeOf $ fni_BSpace r, sizeOf $ fni_CSpace r, sizeOf $ fni_Reserved2 r, sizeOf $ fni_BaseOffset r ]
    recView r = viewRecord (typeOf r) [ viewField "GCGID" (viewString $ fni_GCGID r), viewField "CharacterIncrement" (viewNumber $ fni_CharacterIncrement r), viewField "AscendHeight" (viewNumber $ fni_AscendHeight r), viewField "DescendDepth" (viewNumber $ fni_DescendDepth r), viewField "Reserved1" (viewNumber $ fni_Reserved1 r), viewField "FNMCount" (viewNumber $ fni_FNMCount r), viewField "ASpace" (viewNumber $ fni_ASpace r), viewField "BSpace" (viewNumber $ fni_BSpace r), viewField "CSpace" (viewNumber $ fni_CSpace r), viewField "Reserved2" (viewNumber $ fni_Reserved2 r), viewField "BaseOffset" (viewNumber $ fni_BaseOffset r) ]
    recType r = 0

instance RecData FNI FNI_Data where
    type RecOf FNI_Data = FNI
    type DataOf FNI = FNI_Data
    readData = fni_Data
    writeData r cs = r { fni_Data = cs }


instance Rec FNM where
    recGet = do a01 <- get; a02 <- get; a03 <- getList; return $ FNM a01 a02 a03
    recPut r = do put $ fnm_Type r; put $ fnm_ r; putList $ fnm_Data r; return ()
    recSizeOf r = sum [ sizeOf $ fnm_Type r, sizeOf $ fnm_ r, sizeOf $ fnm_Data r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fnm_Type r), viewField "_" (viewNumber $ fnm_ r), viewField "Data" (viewData $ fnm_Data r) ]
    recType = fromEnum . fnm_Type

instance Rec FNM_Data where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ FNM_Data a01 a02 a03
    recPut r = do put $ fnm_Width r; put $ fnm_Height r; put $ fnm_Offset r; return ()
    recSizeOf r = sum [ sizeOf $ fnm_Width r, sizeOf $ fnm_Height r, sizeOf $ fnm_Offset r ]
    recView r = viewRecord (typeOf r) [ viewField "Width" (viewNumber $ fnm_Width r), viewField "Height" (viewNumber $ fnm_Height r), viewField "Offset" (viewNumber $ fnm_Offset r) ]
    recType r = 0

instance RecData FNM FNM_Data where
    type RecOf FNM_Data = FNM
    type DataOf FNM = FNM_Data
    readData = fnm_Data
    writeData r cs = r { fnm_Data = cs }

instance Rec FNN where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ FNN a01 a02 a03
    recPut r = do put $ fnn_Type r; put $ fnn_ r; put $ fnn r; return ()
    recSizeOf r = sum [ sizeOf $ fnn_Type r, sizeOf $ fnn_ r, sizeOf $ fnn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fnn_Type r), viewField "_" (viewNumber $ fnn_ r), viewField "" (viewNStr $ fnn r) ]
    recType = fromEnum . fnn_Type

instance Rec FNO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; return $ FNO a01 a02 a03 a04 a05 a06 a07 a08
    recPut r = do put $ fno_Type r; put $ fno_ r; put $ fno_Reserved r; put $ fno_CharacterRotation r; put $ fno_MaxBaseOffset r; put $ fno_MaxCharacterIncrement r; put $ fno_SpaceCharacterIncrement r; put $ fno r; return ()
    recSizeOf r = sum [ sizeOf $ fno_Type r, sizeOf $ fno_ r, sizeOf $ fno_Reserved r, sizeOf $ fno_CharacterRotation r, sizeOf $ fno_MaxBaseOffset r, sizeOf $ fno_MaxCharacterIncrement r, sizeOf $ fno_SpaceCharacterIncrement r, sizeOf $ fno r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fno_Type r), viewField "_" (viewNumber $ fno_ r), viewField "Reserved" (viewNumber $ fno_Reserved r), viewField "CharacterRotation" (viewNumber $ fno_CharacterRotation r), viewField "MaxBaseOffset" (viewNumber $ fno_MaxBaseOffset r), viewField "MaxCharacterIncrement" (viewNumber $ fno_MaxCharacterIncrement r), viewField "SpaceCharacterIncrement" (viewNumber $ fno_SpaceCharacterIncrement r), viewField "" (viewNStr $ fno r) ]
    recType = fromEnum . fno_Type

instance Rec FNP where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ FNP a01 a02 a03
    recPut r = do put $ fnp_Type r; put $ fnp_ r; put $ fnp r; return ()
    recSizeOf r = sum [ sizeOf $ fnp_Type r, sizeOf $ fnp_ r, sizeOf $ fnp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ fnp_Type r), viewField "_" (viewNumber $ fnp_ r), viewField "" (viewNStr $ fnp r) ]
    recType = fromEnum . fnp_Type

