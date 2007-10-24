{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstanceAFP () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

instance Rec BAG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BAG a01 a02 a03
    recPut r = do put $ bag_Type r; put $ bag_ r; put $ bag r; return ()
    recSizeOf r = sum [ sizeOf $ bag_Type r, sizeOf $ bag_ r, sizeOf $ bag r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bag_Type r), viewField "_" (viewNumber $ bag_ r), viewField "" (viewNStr $ bag r) ]
    recType = fromEnum . bag_Type

instance Rec BBC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BBC a01 a02 a03
    recPut r = do put $ bbc_Type r; put $ bbc_ r; put $ bbc r; return ()
    recSizeOf r = sum [ sizeOf $ bbc_Type r, sizeOf $ bbc_ r, sizeOf $ bbc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bbc_Type r), viewField "_" (viewNumber $ bbc_ r), viewField "" (viewNStr $ bbc r) ]
    recType = fromEnum . bbc_Type

instance Rec BCA where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BCA a01 a02 a03
    recPut r = do put $ bca_Type r; put $ bca_ r; put $ bca r; return ()
    recSizeOf r = sum [ sizeOf $ bca_Type r, sizeOf $ bca_ r, sizeOf $ bca r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bca_Type r), viewField "_" (viewNumber $ bca_ r), viewField "" (viewNStr $ bca r) ]
    recType = fromEnum . bca_Type

instance Rec BCF where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BCF a01 a02 a03
    recPut r = do put $ bcf_Type r; put $ bcf_ r; put $ bcf r; return ()
    recSizeOf r = sum [ sizeOf $ bcf_Type r, sizeOf $ bcf_ r, sizeOf $ bcf r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bcf_Type r), viewField "_" (viewNumber $ bcf_ r), viewField "" (viewNStr $ bcf r) ]
    recType = fromEnum . bcf_Type

instance Rec BCP where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BCP a01 a02 a03
    recPut r = do put $ bcp_Type r; put $ bcp_ r; put $ bcp r; return ()
    recSizeOf r = sum [ sizeOf $ bcp_Type r, sizeOf $ bcp_ r, sizeOf $ bcp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bcp_Type r), viewField "_" (viewNumber $ bcp_ r), viewField "" (viewNStr $ bcp r) ]
    recType = fromEnum . bcp_Type

instance Rec BDA where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BDA a01 a02 a03
    recPut r = do put $ bda_Type r; put $ bda_ r; put $ bda r; return ()
    recSizeOf r = sum [ sizeOf $ bda_Type r, sizeOf $ bda_ r, sizeOf $ bda r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bda_Type r), viewField "_" (viewNumber $ bda_ r), viewField "" (viewNStr $ bda r) ]
    recType = fromEnum . bda_Type

instance Rec BDD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BDD a01 a02 a03
    recPut r = do put $ bdd_Type r; put $ bdd_ r; put $ bdd r; return ()
    recSizeOf r = sum [ sizeOf $ bdd_Type r, sizeOf $ bdd_ r, sizeOf $ bdd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bdd_Type r), viewField "_" (viewNumber $ bdd_ r), viewField "" (viewNStr $ bdd r) ]
    recType = fromEnum . bdd_Type

instance Rec BDG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BDG a01 a02 a03
    recPut r = do put $ bdg_Type r; put $ bdg_ r; put $ bdg r; return ()
    recSizeOf r = sum [ sizeOf $ bdg_Type r, sizeOf $ bdg_ r, sizeOf $ bdg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bdg_Type r), viewField "_" (viewNumber $ bdg_ r), viewField "" (viewNStr $ bdg r) ]
    recType = fromEnum . bdg_Type

instance Rec BDI where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BDI a01 a02 a03
    recPut r = do put $ bdi_Type r; put $ bdi_ r; put $ bdi r; return ()
    recSizeOf r = sum [ sizeOf $ bdi_Type r, sizeOf $ bdi_ r, sizeOf $ bdi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bdi_Type r), viewField "_" (viewNumber $ bdi_ r), viewField "" (viewNStr $ bdi r) ]
    recType = fromEnum . bdi_Type

instance Rec BDM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BDM a01 a02 a03
    recPut r = do put $ bdm_Type r; put $ bdm_ r; put $ bdm r; return ()
    recSizeOf r = sum [ sizeOf $ bdm_Type r, sizeOf $ bdm_ r, sizeOf $ bdm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bdm_Type r), viewField "_" (viewNumber $ bdm_ r), viewField "" (viewNStr $ bdm r) ]
    recType = fromEnum . bdm_Type

instance Rec BDT where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BDT a01 a02 a03
    recPut r = do put $ bdt_Type r; put $ bdt_ r; put $ bdt r; return ()
    recSizeOf r = sum [ sizeOf $ bdt_Type r, sizeOf $ bdt_ r, sizeOf $ bdt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bdt_Type r), viewField "_" (viewNumber $ bdt_ r), viewField "" (viewNStr $ bdt r) ]
    recType = fromEnum . bdt_Type

instance Rec BDX where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BDX a01 a02 a03
    recPut r = do put $ bdx_Type r; put $ bdx_ r; put $ bdx r; return ()
    recSizeOf r = sum [ sizeOf $ bdx_Type r, sizeOf $ bdx_ r, sizeOf $ bdx r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bdx_Type r), viewField "_" (viewNumber $ bdx_ r), viewField "" (viewNStr $ bdx r) ]
    recType = fromEnum . bdx_Type

instance Rec BFG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BFG a01 a02 a03
    recPut r = do put $ bfg_Type r; put $ bfg_ r; put $ bfg r; return ()
    recSizeOf r = sum [ sizeOf $ bfg_Type r, sizeOf $ bfg_ r, sizeOf $ bfg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bfg_Type r), viewField "_" (viewNumber $ bfg_ r), viewField "" (viewNStr $ bfg r) ]
    recType = fromEnum . bfg_Type

instance Rec BFM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BFM a01 a02 a03
    recPut r = do put $ bfm_Type r; put $ bfm_ r; put $ bfm r; return ()
    recSizeOf r = sum [ sizeOf $ bfm_Type r, sizeOf $ bfm_ r, sizeOf $ bfm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bfm_Type r), viewField "_" (viewNumber $ bfm_ r), viewField "" (viewNStr $ bfm r) ]
    recType = fromEnum . bfm_Type

instance Rec BFN where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BFN a01 a02 a03
    recPut r = do put $ bfn_Type r; put $ bfn_ r; put $ bfn r; return ()
    recSizeOf r = sum [ sizeOf $ bfn_Type r, sizeOf $ bfn_ r, sizeOf $ bfn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bfn_Type r), viewField "_" (viewNumber $ bfn_ r), viewField "" (viewNStr $ bfn r) ]
    recType = fromEnum . bfn_Type

instance Rec BGR where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BGR a01 a02 a03
    recPut r = do put $ bgr_Type r; put $ bgr_ r; put $ bgr r; return ()
    recSizeOf r = sum [ sizeOf $ bgr_Type r, sizeOf $ bgr_ r, sizeOf $ bgr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bgr_Type r), viewField "_" (viewNumber $ bgr_ r), viewField "" (viewNStr $ bgr r) ]
    recType = fromEnum . bgr_Type

instance Rec BII where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BII a01 a02 a03
    recPut r = do put $ bii_Type r; put $ bii_ r; put $ bii_ImageObjectName r; return ()
    recSizeOf r = sum [ sizeOf $ bii_Type r, sizeOf $ bii_ r, sizeOf $ bii_ImageObjectName r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bii_Type r), viewField "_" (viewNumber $ bii_ r), viewField "ImageObjectName" (viewString $ bii_ImageObjectName r) ]
    recType = fromEnum . bii_Type

instance Rec BIM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BIM a01 a02 a03
    recPut r = do put $ bim_Type r; put $ bim_ r; put $ bim r; return ()
    recSizeOf r = sum [ sizeOf $ bim_Type r, sizeOf $ bim_ r, sizeOf $ bim r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bim_Type r), viewField "_" (viewNumber $ bim_ r), viewField "" (viewNStr $ bim r) ]
    recType = fromEnum . bim_Type

instance Rec BMM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BMM a01 a02 a03
    recPut r = do put $ bmm_Type r; put $ bmm_ r; put $ bmm r; return ()
    recSizeOf r = sum [ sizeOf $ bmm_Type r, sizeOf $ bmm_ r, sizeOf $ bmm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bmm_Type r), viewField "_" (viewNumber $ bmm_ r), viewField "" (viewNStr $ bmm r) ]
    recType = fromEnum . bmm_Type

instance Rec BMO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BMO a01 a02 a03
    recPut r = do put $ bmo_Type r; put $ bmo_ r; put $ bmo r; return ()
    recSizeOf r = sum [ sizeOf $ bmo_Type r, sizeOf $ bmo_ r, sizeOf $ bmo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bmo_Type r), viewField "_" (viewNumber $ bmo_ r), viewField "" (viewNStr $ bmo r) ]
    recType = fromEnum . bmo_Type

instance Rec BNG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BNG a01 a02 a03
    recPut r = do put $ bng_Type r; put $ bng_ r; put $ bng r; return ()
    recSizeOf r = sum [ sizeOf $ bng_Type r, sizeOf $ bng_ r, sizeOf $ bng r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bng_Type r), viewField "_" (viewNumber $ bng_ r), viewField "" (viewNStr $ bng r) ]
    recType = fromEnum . bng_Type

instance Rec BOC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BOC a01 a02 a03
    recPut r = do put $ boc_Type r; put $ boc_ r; put $ boc r; return ()
    recSizeOf r = sum [ sizeOf $ boc_Type r, sizeOf $ boc_ r, sizeOf $ boc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ boc_Type r), viewField "_" (viewNumber $ boc_ r), viewField "" (viewNStr $ boc r) ]
    recType = fromEnum . boc_Type

instance Rec BOG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BOG a01 a02 a03
    recPut r = do put $ bog_Type r; put $ bog_ r; put $ bog r; return ()
    recSizeOf r = sum [ sizeOf $ bog_Type r, sizeOf $ bog_ r, sizeOf $ bog r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bog_Type r), viewField "_" (viewNumber $ bog_ r), viewField "" (viewNStr $ bog r) ]
    recType = fromEnum . bog_Type

instance Rec BPG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BPG a01 a02 a03
    recPut r = do put $ bpg_Type r; put $ bpg_ r; put $ bpg r; return ()
    recSizeOf r = sum [ sizeOf $ bpg_Type r, sizeOf $ bpg_ r, sizeOf $ bpg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bpg_Type r), viewField "_" (viewNumber $ bpg_ r), viewField "" (viewNStr $ bpg r) ]
    recType = fromEnum . bpg_Type

instance Rec BPM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BPM a01 a02 a03
    recPut r = do put $ bpm_Type r; put $ bpm_ r; put $ bpm r; return ()
    recSizeOf r = sum [ sizeOf $ bpm_Type r, sizeOf $ bpm_ r, sizeOf $ bpm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bpm_Type r), viewField "_" (viewNumber $ bpm_ r), viewField "" (viewNStr $ bpm r) ]
    recType = fromEnum . bpm_Type

instance Rec BPS where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BPS a01 a02 a03
    recPut r = do put $ bps_Type r; put $ bps_ r; put $ bps r; return ()
    recSizeOf r = sum [ sizeOf $ bps_Type r, sizeOf $ bps_ r, sizeOf $ bps r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bps_Type r), viewField "_" (viewNumber $ bps_ r), viewField "" (viewNStr $ bps r) ]
    recType = fromEnum . bps_Type

instance Rec BPT where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BPT a01 a02 a03
    recPut r = do put $ bpt_Type r; put $ bpt_ r; put $ bpt r; return ()
    recSizeOf r = sum [ sizeOf $ bpt_Type r, sizeOf $ bpt_ r, sizeOf $ bpt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bpt_Type r), viewField "_" (viewNumber $ bpt_ r), viewField "" (viewNStr $ bpt r) ]
    recType = fromEnum . bpt_Type

instance Rec BR where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BR a01 a02 a03
    recPut r = do put $ br_Type r; put $ br_ r; put $ br r; return ()
    recSizeOf r = sum [ sizeOf $ br_Type r, sizeOf $ br_ r, sizeOf $ br r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ br_Type r), viewField "_" (viewNumber $ br_ r), viewField "" (viewNStr $ br r) ]
    recType = fromEnum . br_Type

instance Rec BRG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BRG a01 a02 a03
    recPut r = do put $ brg_Type r; put $ brg_ r; put $ brg r; return ()
    recSizeOf r = sum [ sizeOf $ brg_Type r, sizeOf $ brg_ r, sizeOf $ brg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ brg_Type r), viewField "_" (viewNumber $ brg_ r), viewField "" (viewNStr $ brg r) ]
    recType = fromEnum . brg_Type

instance Rec BSG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ BSG a01 a02 a03
    recPut r = do put $ bsg_Type r; put $ bsg_ r; put $ bsg r; return ()
    recSizeOf r = sum [ sizeOf $ bsg_Type r, sizeOf $ bsg_ r, sizeOf $ bsg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bsg_Type r), viewField "_" (viewNumber $ bsg_ r), viewField "" (viewNStr $ bsg r) ]
    recType = fromEnum . bsg_Type

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

instance RecData CFI CFI_Data where
    readData r = cfi_Data r
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

instance RecData CPI CPI_Data where
    readData r = cpi_Data r
    writeData r cs = r { cpi_Data = cs }

instance Rec CTC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ CTC a01 a02 a03
    recPut r = do put $ ctc_Type r; put $ ctc_ r; put $ ctc r; return ()
    recSizeOf r = sum [ sizeOf $ ctc_Type r, sizeOf $ ctc_ r, sizeOf $ ctc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ctc_Type r), viewField "_" (viewNumber $ ctc_ r), viewField "" (viewNStr $ ctc r) ]
    recType = fromEnum . ctc_Type

instance Rec DXD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ DXD a01 a02 a03
    recPut r = do put $ dxd_Type r; put $ dxd_ r; put $ dxd r; return ()
    recSizeOf r = sum [ sizeOf $ dxd_Type r, sizeOf $ dxd_ r, sizeOf $ dxd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ dxd_Type r), viewField "_" (viewNumber $ dxd_ r), viewField "" (viewNStr $ dxd r) ]
    recType = fromEnum . dxd_Type

instance Rec EAG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EAG a01 a02 a03
    recPut r = do put $ eag_Type r; put $ eag_ r; put $ eag r; return ()
    recSizeOf r = sum [ sizeOf $ eag_Type r, sizeOf $ eag_ r, sizeOf $ eag r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eag_Type r), viewField "_" (viewNumber $ eag_ r), viewField "" (viewNStr $ eag r) ]
    recType = fromEnum . eag_Type

instance Rec EBC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EBC a01 a02 a03
    recPut r = do put $ ebc_Type r; put $ ebc_ r; put $ ebc r; return ()
    recSizeOf r = sum [ sizeOf $ ebc_Type r, sizeOf $ ebc_ r, sizeOf $ ebc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ebc_Type r), viewField "_" (viewNumber $ ebc_ r), viewField "" (viewNStr $ ebc r) ]
    recType = fromEnum . ebc_Type

instance Rec ECA where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ ECA a01 a02 a03
    recPut r = do put $ eca_Type r; put $ eca_ r; put $ eca r; return ()
    recSizeOf r = sum [ sizeOf $ eca_Type r, sizeOf $ eca_ r, sizeOf $ eca r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eca_Type r), viewField "_" (viewNumber $ eca_ r), viewField "" (viewNStr $ eca r) ]
    recType = fromEnum . eca_Type

instance Rec ECF where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ ECF a01 a02 a03
    recPut r = do put $ ecf_Type r; put $ ecf_ r; put $ ecf r; return ()
    recSizeOf r = sum [ sizeOf $ ecf_Type r, sizeOf $ ecf_ r, sizeOf $ ecf r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ecf_Type r), viewField "_" (viewNumber $ ecf_ r), viewField "" (viewNStr $ ecf r) ]
    recType = fromEnum . ecf_Type

instance Rec ECP where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ ECP a01 a02 a03
    recPut r = do put $ ecp_Type r; put $ ecp_ r; put $ ecp r; return ()
    recSizeOf r = sum [ sizeOf $ ecp_Type r, sizeOf $ ecp_ r, sizeOf $ ecp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ecp_Type r), viewField "_" (viewNumber $ ecp_ r), viewField "" (viewNStr $ ecp r) ]
    recType = fromEnum . ecp_Type

instance Rec EDG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EDG a01 a02 a03
    recPut r = do put $ edg_Type r; put $ edg_ r; put $ edg r; return ()
    recSizeOf r = sum [ sizeOf $ edg_Type r, sizeOf $ edg_ r, sizeOf $ edg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ edg_Type r), viewField "_" (viewNumber $ edg_ r), viewField "" (viewNStr $ edg r) ]
    recType = fromEnum . edg_Type

instance Rec EDI where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EDI a01 a02 a03
    recPut r = do put $ edi_Type r; put $ edi_ r; put $ edi r; return ()
    recSizeOf r = sum [ sizeOf $ edi_Type r, sizeOf $ edi_ r, sizeOf $ edi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ edi_Type r), viewField "_" (viewNumber $ edi_ r), viewField "" (viewNStr $ edi r) ]
    recType = fromEnum . edi_Type

instance Rec EDM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EDM a01 a02 a03
    recPut r = do put $ edm_Type r; put $ edm_ r; put $ edm r; return ()
    recSizeOf r = sum [ sizeOf $ edm_Type r, sizeOf $ edm_ r, sizeOf $ edm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ edm_Type r), viewField "_" (viewNumber $ edm_ r), viewField "" (viewNStr $ edm r) ]
    recType = fromEnum . edm_Type

instance Rec EDT where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EDT a01 a02 a03
    recPut r = do put $ edt_Type r; put $ edt_ r; put $ edt r; return ()
    recSizeOf r = sum [ sizeOf $ edt_Type r, sizeOf $ edt_ r, sizeOf $ edt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ edt_Type r), viewField "_" (viewNumber $ edt_ r), viewField "" (viewNStr $ edt r) ]
    recType = fromEnum . edt_Type

instance Rec EDX where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EDX a01 a02 a03
    recPut r = do put $ edx_Type r; put $ edx_ r; put $ edx r; return ()
    recSizeOf r = sum [ sizeOf $ edx_Type r, sizeOf $ edx_ r, sizeOf $ edx r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ edx_Type r), viewField "_" (viewNumber $ edx_ r), viewField "" (viewNStr $ edx r) ]
    recType = fromEnum . edx_Type

instance Rec EFG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EFG a01 a02 a03
    recPut r = do put $ efg_Type r; put $ efg_ r; put $ efg r; return ()
    recSizeOf r = sum [ sizeOf $ efg_Type r, sizeOf $ efg_ r, sizeOf $ efg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ efg_Type r), viewField "_" (viewNumber $ efg_ r), viewField "" (viewNStr $ efg r) ]
    recType = fromEnum . efg_Type

instance Rec EFM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EFM a01 a02 a03
    recPut r = do put $ efm_Type r; put $ efm_ r; put $ efm r; return ()
    recSizeOf r = sum [ sizeOf $ efm_Type r, sizeOf $ efm_ r, sizeOf $ efm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ efm_Type r), viewField "_" (viewNumber $ efm_ r), viewField "" (viewNStr $ efm r) ]
    recType = fromEnum . efm_Type

instance Rec EFN where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EFN a01 a02 a03
    recPut r = do put $ efn_Type r; put $ efn_ r; put $ efn r; return ()
    recSizeOf r = sum [ sizeOf $ efn_Type r, sizeOf $ efn_ r, sizeOf $ efn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ efn_Type r), viewField "_" (viewNumber $ efn_ r), viewField "" (viewNStr $ efn r) ]
    recType = fromEnum . efn_Type

instance Rec EGR where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EGR a01 a02 a03
    recPut r = do put $ egr_Type r; put $ egr_ r; put $ egr r; return ()
    recSizeOf r = sum [ sizeOf $ egr_Type r, sizeOf $ egr_ r, sizeOf $ egr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ egr_Type r), viewField "_" (viewNumber $ egr_ r), viewField "" (viewNStr $ egr r) ]
    recType = fromEnum . egr_Type

instance Rec EII where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EII a01 a02 a03
    recPut r = do put $ eii_Type r; put $ eii_ r; put $ eii_ImageObjectName r; return ()
    recSizeOf r = sum [ sizeOf $ eii_Type r, sizeOf $ eii_ r, sizeOf $ eii_ImageObjectName r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eii_Type r), viewField "_" (viewNumber $ eii_ r), viewField "ImageObjectName" (viewString $ eii_ImageObjectName r) ]
    recType = fromEnum . eii_Type

instance Rec EIM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EIM a01 a02 a03
    recPut r = do put $ eim_Type r; put $ eim_ r; put $ eim r; return ()
    recSizeOf r = sum [ sizeOf $ eim_Type r, sizeOf $ eim_ r, sizeOf $ eim r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eim_Type r), viewField "_" (viewNumber $ eim_ r), viewField "" (viewNStr $ eim r) ]
    recType = fromEnum . eim_Type

instance Rec EMM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EMM a01 a02 a03
    recPut r = do put $ emm_Type r; put $ emm_ r; put $ emm r; return ()
    recSizeOf r = sum [ sizeOf $ emm_Type r, sizeOf $ emm_ r, sizeOf $ emm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ emm_Type r), viewField "_" (viewNumber $ emm_ r), viewField "" (viewNStr $ emm r) ]
    recType = fromEnum . emm_Type

instance Rec EMO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EMO a01 a02 a03
    recPut r = do put $ emo_Type r; put $ emo_ r; put $ emo r; return ()
    recSizeOf r = sum [ sizeOf $ emo_Type r, sizeOf $ emo_ r, sizeOf $ emo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ emo_Type r), viewField "_" (viewNumber $ emo_ r), viewField "" (viewNStr $ emo r) ]
    recType = fromEnum . emo_Type

instance Rec ENG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ ENG a01 a02 a03
    recPut r = do put $ eng_Type r; put $ eng_ r; put $ eng r; return ()
    recSizeOf r = sum [ sizeOf $ eng_Type r, sizeOf $ eng_ r, sizeOf $ eng r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eng_Type r), viewField "_" (viewNumber $ eng_ r), viewField "" (viewNStr $ eng r) ]
    recType = fromEnum . eng_Type

instance Rec EOC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EOC a01 a02 a03
    recPut r = do put $ eoc_Type r; put $ eoc_ r; put $ eoc r; return ()
    recSizeOf r = sum [ sizeOf $ eoc_Type r, sizeOf $ eoc_ r, sizeOf $ eoc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eoc_Type r), viewField "_" (viewNumber $ eoc_ r), viewField "" (viewNStr $ eoc r) ]
    recType = fromEnum . eoc_Type

instance Rec EOG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EOG a01 a02 a03
    recPut r = do put $ eog_Type r; put $ eog_ r; put $ eog r; return ()
    recSizeOf r = sum [ sizeOf $ eog_Type r, sizeOf $ eog_ r, sizeOf $ eog r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eog_Type r), viewField "_" (viewNumber $ eog_ r), viewField "" (viewNStr $ eog r) ]
    recType = fromEnum . eog_Type

instance Rec EPG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EPG a01 a02 a03
    recPut r = do put $ epg_Type r; put $ epg_ r; put $ epg r; return ()
    recSizeOf r = sum [ sizeOf $ epg_Type r, sizeOf $ epg_ r, sizeOf $ epg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ epg_Type r), viewField "_" (viewNumber $ epg_ r), viewField "" (viewNStr $ epg r) ]
    recType = fromEnum . epg_Type

instance Rec EPM where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EPM a01 a02 a03
    recPut r = do put $ epm_Type r; put $ epm_ r; put $ epm r; return ()
    recSizeOf r = sum [ sizeOf $ epm_Type r, sizeOf $ epm_ r, sizeOf $ epm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ epm_Type r), viewField "_" (viewNumber $ epm_ r), viewField "" (viewNStr $ epm r) ]
    recType = fromEnum . epm_Type

instance Rec EPS where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EPS a01 a02 a03
    recPut r = do put $ eps_Type r; put $ eps_ r; put $ eps r; return ()
    recSizeOf r = sum [ sizeOf $ eps_Type r, sizeOf $ eps_ r, sizeOf $ eps r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eps_Type r), viewField "_" (viewNumber $ eps_ r), viewField "" (viewNStr $ eps r) ]
    recType = fromEnum . eps_Type

instance Rec EPT where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ EPT a01 a02 a03
    recPut r = do put $ ept_Type r; put $ ept_ r; put $ ept r; return ()
    recSizeOf r = sum [ sizeOf $ ept_Type r, sizeOf $ ept_ r, sizeOf $ ept r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ept_Type r), viewField "_" (viewNumber $ ept_ r), viewField "" (viewNStr $ ept r) ]
    recType = fromEnum . ept_Type

instance Rec ER where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ ER a01 a02 a03
    recPut r = do put $ er_Type r; put $ er_ r; put $ er r; return ()
    recSizeOf r = sum [ sizeOf $ er_Type r, sizeOf $ er_ r, sizeOf $ er r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ er_Type r), viewField "_" (viewNumber $ er_ r), viewField "" (viewNStr $ er r) ]
    recType = fromEnum . er_Type

instance Rec ERG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ ERG a01 a02 a03
    recPut r = do put $ erg_Type r; put $ erg_ r; put $ erg r; return ()
    recSizeOf r = sum [ sizeOf $ erg_Type r, sizeOf $ erg_ r, sizeOf $ erg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ erg_Type r), viewField "_" (viewNumber $ erg_ r), viewField "" (viewNStr $ erg r) ]
    recType = fromEnum . erg_Type

instance Rec ESG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ ESG a01 a02 a03
    recPut r = do put $ esg_Type r; put $ esg_ r; put $ esg r; return ()
    recSizeOf r = sum [ sizeOf $ esg_Type r, sizeOf $ esg_ r, sizeOf $ esg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ esg_Type r), viewField "_" (viewNumber $ esg_ r), viewField "" (viewNStr $ esg r) ]
    recType = fromEnum . esg_Type

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
    readData r = fni_Data r
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
    readData r = fnm_Data r
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

instance Rec GAD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ GAD a01 a02 a03
    recPut r = do put $ gad_Type r; put $ gad_ r; put $ gad r; return ()
    recSizeOf r = sum [ sizeOf $ gad_Type r, sizeOf $ gad_ r, sizeOf $ gad r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ gad_Type r), viewField "_" (viewNumber $ gad_ r), viewField "" (viewNStr $ gad r) ]
    recType = fromEnum . gad_Type

instance Rec GDD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ GDD a01 a02 a03
    recPut r = do put $ gdd_Type r; put $ gdd_ r; put $ gdd r; return ()
    recSizeOf r = sum [ sizeOf $ gdd_Type r, sizeOf $ gdd_ r, sizeOf $ gdd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ gdd_Type r), viewField "_" (viewNumber $ gdd_ r), viewField "" (viewNStr $ gdd r) ]
    recType = fromEnum . gdd_Type

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
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ips_Type r), viewField "_" (viewNumber $ ips_ r), viewField "" (viewNStr $ ips r) ]
    recType = fromEnum . ips_Type

instance Rec IRD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ IRD a01 a02 a03
    recPut r = do put $ ird_Type r; put $ ird_ r; put $ ird_ImageData r; return ()
    recSizeOf r = sum [ sizeOf $ ird_Type r, sizeOf $ ird_ r, sizeOf $ ird_ImageData r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ird_Type r), viewField "_" (viewNumber $ ird_ r), viewField "ImageData" (viewNStr $ ird_ImageData r) ]
    recType = fromEnum . ird_Type

instance Rec LLE where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ LLE a01 a02 a03
    recPut r = do put $ lle_Type r; put $ lle_ r; put $ lle r; return ()
    recSizeOf r = sum [ sizeOf $ lle_Type r, sizeOf $ lle_ r, sizeOf $ lle r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ lle_Type r), viewField "_" (viewNumber $ lle_ r), viewField "" (viewNStr $ lle r) ]
    recType = fromEnum . lle_Type

instance Rec LNC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ LNC a01 a02 a03
    recPut r = do put $ lnc_Type r; put $ lnc_ r; put $ lnc r; return ()
    recSizeOf r = sum [ sizeOf $ lnc_Type r, sizeOf $ lnc_ r, sizeOf $ lnc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ lnc_Type r), viewField "_" (viewNumber $ lnc_ r), viewField "" (viewNStr $ lnc r) ]
    recType = fromEnum . lnc_Type

instance Rec LND where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ LND a01 a02 a03
    recPut r = do put $ lnd_Type r; put $ lnd_ r; put $ lnd r; return ()
    recSizeOf r = sum [ sizeOf $ lnd_Type r, sizeOf $ lnd_ r, sizeOf $ lnd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ lnd_Type r), viewField "_" (viewNumber $ lnd_ r), viewField "" (viewNStr $ lnd r) ]
    recType = fromEnum . lnd_Type

instance Rec MBC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MBC a01 a02 a03
    recPut r = do put $ mbc_Type r; put $ mbc_ r; put $ mbc r; return ()
    recSizeOf r = sum [ sizeOf $ mbc_Type r, sizeOf $ mbc_ r, sizeOf $ mbc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mbc_Type r), viewField "_" (viewNumber $ mbc_ r), viewField "" (viewNStr $ mbc r) ]
    recType = fromEnum . mbc_Type

instance Rec MCA where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MCA a01 a02 a03
    recPut r = do put $ mca_Type r; put $ mca_ r; put $ mca r; return ()
    recSizeOf r = sum [ sizeOf $ mca_Type r, sizeOf $ mca_ r, sizeOf $ mca r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mca_Type r), viewField "_" (viewNumber $ mca_ r), viewField "" (viewNStr $ mca r) ]
    recType = fromEnum . mca_Type

instance Rec MCC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MCC a01 a02 a03
    recPut r = do put $ mcc_Type r; put $ mcc_ r; put $ mcc r; return ()
    recSizeOf r = sum [ sizeOf $ mcc_Type r, sizeOf $ mcc_ r, sizeOf $ mcc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mcc_Type r), viewField "_" (viewNumber $ mcc_ r), viewField "" (viewNStr $ mcc r) ]
    recType = fromEnum . mcc_Type

instance Rec MCD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MCD a01 a02 a03
    recPut r = do put $ mcd_Type r; put $ mcd_ r; put $ mcd r; return ()
    recSizeOf r = sum [ sizeOf $ mcd_Type r, sizeOf $ mcd_ r, sizeOf $ mcd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mcd_Type r), viewField "_" (viewNumber $ mcd_ r), viewField "" (viewNStr $ mcd r) ]
    recType = fromEnum . mcd_Type

