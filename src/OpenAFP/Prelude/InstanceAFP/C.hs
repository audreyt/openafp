
module OpenAFP.Prelude.InstanceAFP.C () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

instance Rec CAT where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ CAT a01 a02 a03
    recPut r = do put $ cat_Type r; put $ cat_ r; put $ cat r; return ()
    recSizeOf r = sum [ sizeOf $ cat_Type r, sizeOf $ cat_ r, sizeOf $ cat r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ cat_Type r), viewField "_" (viewNumber $ cat_ r), viewField "" (viewNStr $ cat r) ]
    recType = fromEnum . cat_Type

instance Rec CDD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ CDD a01 a02 a03
    recPut r = do put $ cdd_Type r; put $ cdd_ r; put $ cdd r; return ()
    recSizeOf r = sum [ sizeOf $ cdd_Type r, sizeOf $ cdd_ r, sizeOf $ cdd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ cdd_Type r), viewField "_" (viewNumber $ cdd_ r), viewField "" (viewNStr $ cdd r) ]
    recType = fromEnum . cdd_Type

instance Rec CFC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; return $ CFC a01 a02 a03 a04
    recPut r = do put $ cfc_Type r; put $ cfc_ r; put $ cfc_CFIRepeatingGroupLength r; put $ cfc r; return ()
    recSizeOf r = sum [ sizeOf $ cfc_Type r, sizeOf $ cfc_ r, sizeOf $ cfc_CFIRepeatingGroupLength r, sizeOf $ cfc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ cfc_Type r), viewField "_" (viewNumber $ cfc_ r), viewField "CFIRepeatingGroupLength" (viewNumber $ cfc_CFIRepeatingGroupLength r), viewField "" (viewNStr $ cfc r) ]
    recType = fromEnum . cfc_Type

instance Rec CFI where
    recGet = do a01 <- get; a02 <- get; a03 <- getList; return $ CFI a01 a02 a03
    recPut r = do put $ cfi_Type r; put $ cfi_ r; putList $ cfi_Data r; return ()
    recSizeOf r = sum [ sizeOf $ cfi_Type r, sizeOf $ cfi_ r, sizeOf $ cfi_Data r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ cfi_Type r), viewField "_" (viewNumber $ cfi_ r), viewField "Data" (viewData $ cfi_Data r) ]
    recType = fromEnum . cfi_Type

instance Rec CFI_Data where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; return $ CFI_Data a01 a02 a03 a04
    recPut r = do put $ cfi_FontCharacterSetName r; put $ cfi_CodePageName r; put $ cfi_CodedFontName r; put $ cfi_Section r; return ()
    recSizeOf r = sum [ sizeOf $ cfi_FontCharacterSetName r, sizeOf $ cfi_CodePageName r, sizeOf $ cfi_CodedFontName r, sizeOf $ cfi_Section r ]
    recView r = viewRecord (typeOf r) [ viewField "FontCharacterSetName" (viewString $ cfi_FontCharacterSetName r), viewField "CodePageName" (viewString $ cfi_CodePageName r), viewField "CodedFontName" (viewString $ cfi_CodedFontName r), viewField "Section" (viewNumber $ cfi_Section r) ]
    recType r = 0

type instance RecOf CFI_Data = CFI
instance RecData CFI where
    type DataOf CFI = CFI_Data
    readData = cfi_Data
    writeData r cs = r { cfi_Data = cs }

instance Rec CPC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; return $ CPC a01 a02 a03 a04 a05 a06 a07 a08
    recPut r = do put $ cpc_Type r; put $ cpc_ r; put $ cpc_GCGID r; put $ cpc_UseFlags r; put $ cpc_CPIRepeatingGroupLength r; put $ cpc_SpaceCharacterSection r; put $ cpc_UseFlags2 r; put $ cpc r; return ()
    recSizeOf r = sum [ sizeOf $ cpc_Type r, sizeOf $ cpc_ r, sizeOf $ cpc_GCGID r, sizeOf $ cpc_UseFlags r, sizeOf $ cpc_CPIRepeatingGroupLength r, sizeOf $ cpc_SpaceCharacterSection r, sizeOf $ cpc_UseFlags2 r, sizeOf $ cpc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ cpc_Type r), viewField "_" (viewNumber $ cpc_ r), viewField "GCGID" (viewString $ cpc_GCGID r), viewField "UseFlags" (viewNumber $ cpc_UseFlags r), viewField "CPIRepeatingGroupLength" (viewNumber $ cpc_CPIRepeatingGroupLength r), viewField "SpaceCharacterSection" (viewNumber $ cpc_SpaceCharacterSection r), viewField "UseFlags2" (viewNumber $ cpc_UseFlags2 r), viewField "" (viewNStr $ cpc r) ]
    recType = fromEnum . cpc_Type

instance Rec CPD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ CPD a01 a02 a03
    recPut r = do put $ cpd_Type r; put $ cpd_ r; put $ cpd r; return ()
    recSizeOf r = sum [ sizeOf $ cpd_Type r, sizeOf $ cpd_ r, sizeOf $ cpd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ cpd_Type r), viewField "_" (viewNumber $ cpd_ r), viewField "" (viewNStr $ cpd r) ]
    recType = fromEnum . cpd_Type

instance Rec CPI where
    recGet = do a01 <- get; a02 <- get; a03 <- getList; return $ CPI a01 a02 a03
    recPut r = do put $ cpi_Type r; put $ cpi_ r; putList $ cpi_Data r; return ()
    recSizeOf r = sum [ sizeOf $ cpi_Type r, sizeOf $ cpi_ r, sizeOf $ cpi_Data r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ cpi_Type r), viewField "_" (viewNumber $ cpi_ r), viewField "Data" (viewData $ cpi_Data r) ]
    recType = fromEnum . cpi_Type

instance Rec CPI_Data where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ CPI_Data a01 a02 a03
    recPut r = do put $ cpi_GCGID r; put $ cpi_Section r; put $ cpi_CodePoint r; return ()
    recSizeOf r = sum [ sizeOf $ cpi_GCGID r, sizeOf $ cpi_Section r, sizeOf $ cpi_CodePoint r ]
    recView r = viewRecord (typeOf r) [ viewField "GCGID" (viewString $ cpi_GCGID r), viewField "Section" (viewNumber $ cpi_Section r), viewField "CodePoint" (viewNumber $ cpi_CodePoint r) ]
    recType r = 0

type instance RecOf CPI_Data = CPI
instance RecData CPI where
    type DataOf CPI = CPI_Data
    readData = cpi_Data
    writeData r cs = r { cpi_Data = cs }

instance Rec CTC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ CTC a01 a02 a03
    recPut r = do put $ ctc_Type r; put $ ctc_ r; put $ ctc r; return ()
    recSizeOf r = sum [ sizeOf $ ctc_Type r, sizeOf $ ctc_ r, sizeOf $ ctc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ctc_Type r), viewField "_" (viewNumber $ ctc_ r), viewField "" (viewNStr $ ctc r) ]
    recType = fromEnum . ctc_Type

