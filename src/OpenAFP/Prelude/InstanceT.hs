{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstanceT () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

apply :: (ChunkBuf c n b, Rec r) => c -> (r -> t) -> t
apply c f = f (decodeChunk c)

instance Rec T_AD where
    recGet = do a01 <- get; a02 <- get; return $ T_AD a01 a02
    recPut r = do put $ t_ad_Type r; put $ t_ad r; return ()
    recSizeOf r = sum [ sizeOf $ t_ad_Type r, sizeOf $ t_ad r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ad_Type r), viewField "" (viewNStr $ t_ad r) ]
    recType = fromEnum . t_ad_Type

instance Rec T_AQ where
    recGet = do a01 <- get; a02 <- get; return $ T_AQ a01 a02
    recPut r = do put $ t_aq_Type r; put $ t_aq r; return ()
    recSizeOf r = sum [ sizeOf $ t_aq_Type r, sizeOf $ t_aq r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_aq_Type r), viewField "" (viewNStr $ t_aq r) ]
    recType = fromEnum . t_aq_Type

instance Rec T_AV where
    recGet = do a01 <- get; a02 <- get; return $ T_AV a01 a02
    recPut r = do put $ t_av_Type r; put $ t_av r; return ()
    recSizeOf r = sum [ sizeOf $ t_av_Type r, sizeOf $ t_av r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_av_Type r), viewField "" (viewNStr $ t_av r) ]
    recType = fromEnum . t_av_Type

instance Rec T_C where
    recGet = do a01 <- get; a02 <- get; return $ T_C a01 a02
    recPut r = do put $ t_c_Type r; put $ t_c r; return ()
    recSizeOf r = sum [ sizeOf $ t_c_Type r, sizeOf $ t_c r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_c_Type r), viewField "" (viewNStr $ t_c r) ]
    recType = fromEnum . t_c_Type

instance Rec T_CF where
    recGet = do a01 <- get; a02 <- get; return $ T_CF a01 a02
    recPut r = do put $ t_cf_Type r; put $ t_cf r; return ()
    recSizeOf r = sum [ sizeOf $ t_cf_Type r, sizeOf $ t_cf r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_cf_Type r), viewField "" (viewNStr $ t_cf r) ]
    recType = fromEnum . t_cf_Type

instance Rec T_CGCSGI where
    recGet = do a01 <- get; a02 <- get; return $ T_CGCSGI a01 a02
    recPut r = do put $ t_cgcsgi_Type r; put $ t_cgcsgi r; return ()
    recSizeOf r = sum [ sizeOf $ t_cgcsgi_Type r, sizeOf $ t_cgcsgi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_cgcsgi_Type r), viewField "" (viewNStr $ t_cgcsgi r) ]
    recType = fromEnum . t_cgcsgi_Type

instance Rec T_CR where
    recGet = do a01 <- get; a02 <- get; return $ T_CR a01 a02
    recPut r = do put $ t_cr_Type r; put $ t_cr r; return ()
    recSizeOf r = sum [ sizeOf $ t_cr_Type r, sizeOf $ t_cr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_cr_Type r), viewField "" (viewNStr $ t_cr r) ]
    recType = fromEnum . t_cr_Type

instance Rec T_CS where
    recGet = do a01 <- get; a02 <- get; return $ T_CS a01 a02
    recPut r = do put $ t_cs_Type r; put $ t_cs r; return ()
    recSizeOf r = sum [ sizeOf $ t_cs_Type r, sizeOf $ t_cs r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_cs_Type r), viewField "" (viewNStr $ t_cs r) ]
    recType = fromEnum . t_cs_Type

instance Rec T_DP where
    recGet = do a01 <- get; a02 <- get; return $ T_DP a01 a02
    recPut r = do put $ t_dp_Type r; put $ t_dp r; return ()
    recSizeOf r = sum [ sizeOf $ t_dp_Type r, sizeOf $ t_dp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_dp_Type r), viewField "" (viewNStr $ t_dp r) ]
    recType = fromEnum . t_dp_Type

instance Rec T_EF where
    recGet = do a01 <- get; a02 <- get; return $ T_EF a01 a02
    recPut r = do put $ t_ef_Type r; put $ t_ef r; return ()
    recSizeOf r = sum [ sizeOf $ t_ef_Type r, sizeOf $ t_ef r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ef_Type r), viewField "" (viewNStr $ t_ef r) ]
    recType = fromEnum . t_ef_Type

instance Rec T_ERLI where
    recGet = do a01 <- get; a02 <- get; return $ T_ERLI a01 a02
    recPut r = do put $ t_erli_Type r; put $ t_erli r; return ()
    recSizeOf r = sum [ sizeOf $ t_erli_Type r, sizeOf $ t_erli r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_erli_Type r), viewField "" (viewNStr $ t_erli r) ]
    recType = fromEnum . t_erli_Type

instance Rec T_ESI where
    recGet = do a01 <- get; a02 <- get; return $ T_ESI a01 a02
    recPut r = do put $ t_esi_Type r; put $ t_esi r; return ()
    recSizeOf r = sum [ sizeOf $ t_esi_Type r, sizeOf $ t_esi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_esi_Type r), viewField "" (viewNStr $ t_esi r) ]
    recType = fromEnum . t_esi_Type

instance Rec T_FCGCSGI where
    recGet = do a01 <- get; a02 <- get; return $ T_FCGCSGI a01 a02
    recPut r = do put $ t_fcgcsgi_Type r; put $ t_fcgcsgi r; return ()
    recSizeOf r = sum [ sizeOf $ t_fcgcsgi_Type r, sizeOf $ t_fcgcsgi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fcgcsgi_Type r), viewField "" (viewNStr $ t_fcgcsgi r) ]
    recType = fromEnum . t_fcgcsgi_Type

instance Rec T_FDS where
    recGet = do a01 <- get; a02 <- get; return $ T_FDS a01 a02
    recPut r = do put $ t_fds_Type r; put $ t_fds r; return ()
    recSizeOf r = sum [ sizeOf $ t_fds_Type r, sizeOf $ t_fds r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fds_Type r), viewField "" (viewNStr $ t_fds r) ]
    recType = fromEnum . t_fds_Type

instance Rec T_FF where
    recGet = do a01 <- get; a02 <- get; return $ T_FF a01 a02
    recPut r = do put $ t_ff_Type r; put $ t_ff r; return ()
    recSizeOf r = sum [ sizeOf $ t_ff_Type r, sizeOf $ t_ff r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ff_Type r), viewField "" (viewNStr $ t_ff r) ]
    recType = fromEnum . t_ff_Type

instance Rec T_FHSF where
    recGet = do a01 <- get; a02 <- get; return $ T_FHSF a01 a02
    recPut r = do put $ t_fhsf_Type r; put $ t_fhsf r; return ()
    recSizeOf r = sum [ sizeOf $ t_fhsf_Type r, sizeOf $ t_fhsf r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fhsf_Type r), viewField "" (viewNStr $ t_fhsf r) ]
    recType = fromEnum . t_fhsf_Type

instance Rec T_FO where
    recGet = do a01 <- get; a02 <- get; return $ T_FO a01 a02
    recPut r = do put $ t_fo_Type r; put $ t_fo r; return ()
    recSizeOf r = sum [ sizeOf $ t_fo_Type r, sizeOf $ t_fo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fo_Type r), viewField "" (viewNStr $ t_fo r) ]
    recType = fromEnum . t_fo_Type

instance Rec T_FQN where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; return $ T_FQN a01 a02 a03 a04
    recPut r = do put $ t_fqn_Type r; put $ t_fqn_SubType r; put $ t_fqn_Format r; put $ t_fqn r; return ()
    recSizeOf r = sum [ sizeOf $ t_fqn_Type r, sizeOf $ t_fqn_SubType r, sizeOf $ t_fqn_Format r, sizeOf $ t_fqn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fqn_Type r), viewField "SubType" (viewNumber $ t_fqn_SubType r), viewField "Format" (viewNumber $ t_fqn_Format r), viewField "" (viewAStr $ t_fqn r) ]
    recType = fromEnum . t_fqn_Type

instance Rec T_FRMT where
    recGet = do a01 <- get; a02 <- get; return $ T_FRMT a01 a02
    recPut r = do put $ t_frmt_Type r; put $ t_frmt r; return ()
    recSizeOf r = sum [ sizeOf $ t_frmt_Type r, sizeOf $ t_frmt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_frmt_Type r), viewField "" (viewNStr $ t_frmt r) ]
    recType = fromEnum . t_frmt_Type

instance Rec T_II where
    recGet = do a01 <- get; a02 <- get; return $ T_II a01 a02
    recPut r = do put $ t_ii_Type r; put $ t_ii r; return ()
    recSizeOf r = sum [ sizeOf $ t_ii_Type r, sizeOf $ t_ii r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ii_Type r), viewField "" (viewNStr $ t_ii r) ]
    recType = fromEnum . t_ii_Type

instance Rec T_LDOPM where
    recGet = do a01 <- get; a02 <- get; return $ T_LDOPM a01 a02
    recPut r = do put $ t_ldopm_Type r; put $ t_ldopm r; return ()
    recSizeOf r = sum [ sizeOf $ t_ldopm_Type r, sizeOf $ t_ldopm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ldopm_Type r), viewField "" (viewNStr $ t_ldopm r) ]
    recType = fromEnum . t_ldopm_Type

instance Rec T_LDTS where
    recGet = do a01 <- get; a02 <- get; return $ T_LDTS a01 a02
    recPut r = do put $ t_ldts_Type r; put $ t_ldts r; return ()
    recSizeOf r = sum [ sizeOf $ t_ldts_Type r, sizeOf $ t_ldts r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ldts_Type r), viewField "" (viewNStr $ t_ldts r) ]
    recType = fromEnum . t_ldts_Type

instance Rec T_MA where
    recGet = do a01 <- get; a02 <- get; return $ T_MA a01 a02
    recPut r = do put $ t_ma_Type r; put $ t_ma r; return ()
    recSizeOf r = sum [ sizeOf $ t_ma_Type r, sizeOf $ t_ma r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ma_Type r), viewField "" (viewNStr $ t_ma r) ]
    recType = fromEnum . t_ma_Type

instance Rec T_MEC where
    recGet = do a01 <- get; a02 <- get; return $ T_MEC a01 a02
    recPut r = do put $ t_mec_Type r; put $ t_mec r; return ()
    recSizeOf r = sum [ sizeOf $ t_mec_Type r, sizeOf $ t_mec r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mec_Type r), viewField "" (viewNStr $ t_mec r) ]
    recType = fromEnum . t_mec_Type

instance Rec T_MF where
    recGet = do a01 <- get; a02 <- get; return $ T_MF a01 a02
    recPut r = do put $ t_mf_Type r; put $ t_mf r; return ()
    recSizeOf r = sum [ sizeOf $ t_mf_Type r, sizeOf $ t_mf r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mf_Type r), viewField "" (viewNStr $ t_mf r) ]
    recType = fromEnum . t_mf_Type

instance Rec T_MIS where
    recGet = do a01 <- get; a02 <- get; return $ T_MIS a01 a02
    recPut r = do put $ t_mis_Type r; put $ t_mis r; return ()
    recSizeOf r = sum [ sizeOf $ t_mis_Type r, sizeOf $ t_mis r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mis_Type r), viewField "" (viewNStr $ t_mis r) ]
    recType = fromEnum . t_mis_Type

instance Rec T_MMPN where
    recGet = do a01 <- get; a02 <- get; return $ T_MMPN a01 a02
    recPut r = do put $ t_mmpn_Type r; put $ t_mmpn r; return ()
    recSizeOf r = sum [ sizeOf $ t_mmpn_Type r, sizeOf $ t_mmpn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mmpn_Type r), viewField "" (viewNStr $ t_mmpn r) ]
    recType = fromEnum . t_mmpn_Type

instance Rec T_MO where
    recGet = do a01 <- get; a02 <- get; return $ T_MO a01 a02
    recPut r = do put $ t_mo_Type r; put $ t_mo r; return ()
    recSizeOf r = sum [ sizeOf $ t_mo_Type r, sizeOf $ t_mo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mo_Type r), viewField "" (viewNStr $ t_mo r) ]
    recType = fromEnum . t_mo_Type

instance Rec T_MOR where
    recGet = do a01 <- get; a02 <- get; return $ T_MOR a01 a02
    recPut r = do put $ t_mor_Type r; put $ t_mor r; return ()
    recSizeOf r = sum [ sizeOf $ t_mor_Type r, sizeOf $ t_mor r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mor_Type r), viewField "" (viewNStr $ t_mor r) ]
    recType = fromEnum . t_mor_Type

instance Rec T_OAMU where
    recGet = do a01 <- get; a02 <- get; return $ T_OAMU a01 a02
    recPut r = do put $ t_oamu_Type r; put $ t_oamu r; return ()
    recSizeOf r = sum [ sizeOf $ t_oamu_Type r, sizeOf $ t_oamu r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_oamu_Type r), viewField "" (viewNStr $ t_oamu r) ]
    recType = fromEnum . t_oamu_Type

instance Rec T_OAS where
    recGet = do a01 <- get; a02 <- get; return $ T_OAS a01 a02
    recPut r = do put $ t_oas_Type r; put $ t_oas r; return ()
    recSizeOf r = sum [ sizeOf $ t_oas_Type r, sizeOf $ t_oas r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_oas_Type r), viewField "" (viewNStr $ t_oas r) ]
    recType = fromEnum . t_oas_Type

instance Rec T_OBE where
    recGet = do a01 <- get; a02 <- get; return $ T_OBE a01 a02
    recPut r = do put $ t_obe_Type r; put $ t_obe r; return ()
    recSizeOf r = sum [ sizeOf $ t_obe_Type r, sizeOf $ t_obe r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_obe_Type r), viewField "" (viewNStr $ t_obe r) ]
    recType = fromEnum . t_obe_Type

instance Rec T_OBO where
    recGet = do a01 <- get; a02 <- get; return $ T_OBO a01 a02
    recPut r = do put $ t_obo_Type r; put $ t_obo r; return ()
    recSizeOf r = sum [ sizeOf $ t_obo_Type r, sizeOf $ t_obo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_obo_Type r), viewField "" (viewNStr $ t_obo r) ]
    recType = fromEnum . t_obo_Type

instance Rec T_OCH where
    recGet = do a01 <- get; a02 <- get; return $ T_OCH a01 a02
    recPut r = do put $ t_och_Type r; put $ t_och r; return ()
    recSizeOf r = sum [ sizeOf $ t_och_Type r, sizeOf $ t_och r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_och_Type r), viewField "" (viewNStr $ t_och r) ]
    recType = fromEnum . t_och_Type

instance Rec T_OCL where
    recGet = do a01 <- get; a02 <- get; return $ T_OCL a01 a02
    recPut r = do put $ t_ocl_Type r; put $ t_ocl r; return ()
    recSizeOf r = sum [ sizeOf $ t_ocl_Type r, sizeOf $ t_ocl r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ocl_Type r), viewField "" (viewNStr $ t_ocl r) ]
    recType = fromEnum . t_ocl_Type

instance Rec T_OCO where
    recGet = do a01 <- get; a02 <- get; return $ T_OCO a01 a02
    recPut r = do put $ t_oco_Type r; put $ t_oco r; return ()
    recSizeOf r = sum [ sizeOf $ t_oco_Type r, sizeOf $ t_oco r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_oco_Type r), viewField "" (viewNStr $ t_oco r) ]
    recType = fromEnum . t_oco_Type

instance Rec T_OFSS where
    recGet = do a01 <- get; a02 <- get; return $ T_OFSS a01 a02
    recPut r = do put $ t_ofss_Type r; put $ t_ofss r; return ()
    recSizeOf r = sum [ sizeOf $ t_ofss_Type r, sizeOf $ t_ofss r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ofss_Type r), viewField "" (viewNStr $ t_ofss r) ]
    recType = fromEnum . t_ofss_Type

instance Rec T_OO where
    recGet = do a01 <- get; a02 <- get; return $ T_OO a01 a02
    recPut r = do put $ t_oo_Type r; put $ t_oo r; return ()
    recSizeOf r = sum [ sizeOf $ t_oo_Type r, sizeOf $ t_oo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_oo_Type r), viewField "" (viewNStr $ t_oo r) ]
    recType = fromEnum . t_oo_Type

instance Rec T_OOI where
    recGet = do a01 <- get; a02 <- get; return $ T_OOI a01 a02
    recPut r = do put $ t_ooi_Type r; put $ t_ooi r; return ()
    recSizeOf r = sum [ sizeOf $ t_ooi_Type r, sizeOf $ t_ooi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ooi_Type r), viewField "" (viewNStr $ t_ooi r) ]
    recType = fromEnum . t_ooi_Type

instance Rec T_OSFE where
    recGet = do a01 <- get; a02 <- get; return $ T_OSFE a01 a02
    recPut r = do put $ t_osfe_Type r; put $ t_osfe r; return ()
    recSizeOf r = sum [ sizeOf $ t_osfe_Type r, sizeOf $ t_osfe r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_osfe_Type r), viewField "" (viewNStr $ t_osfe r) ]
    recType = fromEnum . t_osfe_Type

instance Rec T_OSFO where
    recGet = do a01 <- get; a02 <- get; return $ T_OSFO a01 a02
    recPut r = do put $ t_osfo_Type r; put $ t_osfo r; return ()
    recSizeOf r = sum [ sizeOf $ t_osfo_Type r, sizeOf $ t_osfo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_osfo_Type r), viewField "" (viewNStr $ t_osfo r) ]
    recType = fromEnum . t_osfo_Type

instance Rec T_PC where
    recGet = do a01 <- get; a02 <- get; return $ T_PC a01 a02
    recPut r = do put $ t_pc_Type r; put $ t_pc r; return ()
    recSizeOf r = sum [ sizeOf $ t_pc_Type r, sizeOf $ t_pc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_pc_Type r), viewField "" (viewNStr $ t_pc r) ]
    recType = fromEnum . t_pc_Type

instance Rec T_POCP where
    recGet = do a01 <- get; a02 <- get; return $ T_POCP a01 a02
    recPut r = do put $ t_pocp_Type r; put $ t_pocp r; return ()
    recSizeOf r = sum [ sizeOf $ t_pocp_Type r, sizeOf $ t_pocp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_pocp_Type r), viewField "" (viewNStr $ t_pocp r) ]
    recType = fromEnum . t_pocp_Type

instance Rec T_PPI where
    recGet = do a01 <- get; a02 <- get; return $ T_PPI a01 a02
    recPut r = do put $ t_ppi_Type r; put $ t_ppi r; return ()
    recSizeOf r = sum [ sizeOf $ t_ppi_Type r, sizeOf $ t_ppi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ppi_Type r), viewField "" (viewNStr $ t_ppi r) ]
    recType = fromEnum . t_ppi_Type

instance Rec T_PSMR where
    recGet = do a01 <- get; a02 <- get; return $ T_PSMR a01 a02
    recPut r = do put $ t_psmr_Type r; put $ t_psmr r; return ()
    recSizeOf r = sum [ sizeOf $ t_psmr_Type r, sizeOf $ t_psmr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_psmr_Type r), viewField "" (viewNStr $ t_psmr r) ]
    recType = fromEnum . t_psmr_Type

instance Rec T_PSRM where
    recGet = do a01 <- get; a02 <- get; return $ T_PSRM a01 a02
    recPut r = do put $ t_psrm_Type r; put $ t_psrm r; return ()
    recSizeOf r = sum [ sizeOf $ t_psrm_Type r, sizeOf $ t_psrm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_psrm_Type r), viewField "" (viewNStr $ t_psrm r) ]
    recType = fromEnum . t_psrm_Type

instance Rec T_PV where
    recGet = do a01 <- get; a02 <- get; return $ T_PV a01 a02
    recPut r = do put $ t_pv_Type r; put $ t_pv r; return ()
    recSizeOf r = sum [ sizeOf $ t_pv_Type r, sizeOf $ t_pv r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_pv_Type r), viewField "" (viewNStr $ t_pv r) ]
    recType = fromEnum . t_pv_Type

instance Rec T_RLI where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ T_RLI a01 a02 a03
    recPut r = do put $ t_rli_Type r; put $ t_rli_SubType r; put $ t_rli r; return ()
    recSizeOf r = sum [ sizeOf $ t_rli_Type r, sizeOf $ t_rli_SubType r, sizeOf $ t_rli r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_rli_Type r), viewField "SubType" (viewNumber $ t_rli_SubType r), viewField "" (viewNumber $ t_rli r) ]
    recType = fromEnum . t_rli_Type

instance Rec T_ROI where
    recGet = do a01 <- get; a02 <- get; return $ T_ROI a01 a02
    recPut r = do put $ t_roi_Type r; put $ t_roi r; return ()
    recSizeOf r = sum [ sizeOf $ t_roi_Type r, sizeOf $ t_roi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_roi_Type r), viewField "" (viewNStr $ t_roi r) ]
    recType = fromEnum . t_roi_Type

instance Rec T_ROT where
    recGet = do a01 <- get; a02 <- get; return $ T_ROT a01 a02
    recPut r = do put $ t_rot_Type r; put $ t_rot r; return ()
    recSizeOf r = sum [ sizeOf $ t_rot_Type r, sizeOf $ t_rot r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_rot_Type r), viewField "" (viewNStr $ t_rot r) ]
    recType = fromEnum . t_rot_Type

instance Rec T_RSN where
    recGet = do a01 <- get; a02 <- get; return $ T_RSN a01 a02
    recPut r = do put $ t_rsn_Type r; put $ t_rsn r; return ()
    recSizeOf r = sum [ sizeOf $ t_rsn_Type r, sizeOf $ t_rsn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_rsn_Type r), viewField "" (viewNStr $ t_rsn r) ]
    recType = fromEnum . t_rsn_Type

instance Rec T_RUA where
    recGet = do a01 <- get; a02 <- get; return $ T_RUA a01 a02
    recPut r = do put $ t_rua_Type r; put $ t_rua r; return ()
    recSizeOf r = sum [ sizeOf $ t_rua_Type r, sizeOf $ t_rua r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_rua_Type r), viewField "" (viewNStr $ t_rua r) ]
    recType = fromEnum . t_rua_Type

instance Rec T_T1CRMT where
    recGet = do a01 <- get; a02 <- get; return $ T_T1CRMT a01 a02
    recPut r = do put $ t_t1crmt_Type r; put $ t_t1crmt r; return ()
    recSizeOf r = sum [ sizeOf $ t_t1crmt_Type r, sizeOf $ t_t1crmt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_t1crmt_Type r), viewField "" (viewNStr $ t_t1crmt r) ]
    recType = fromEnum . t_t1crmt_Type

instance Rec T_T2FRMT where
    recGet = do a01 <- get; a02 <- get; return $ T_T2FRMT a01 a02
    recPut r = do put $ t_t2frmt_Type r; put $ t_t2frmt r; return ()
    recSizeOf r = sum [ sizeOf $ t_t2frmt_Type r, sizeOf $ t_t2frmt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_t2frmt_Type r), viewField "" (viewNStr $ t_t2frmt r) ]
    recType = fromEnum . t_t2frmt_Type

instance Rec T_TO where
    recGet = do a01 <- get; a02 <- get; return $ T_TO a01 a02
    recPut r = do put $ t_to_Type r; put $ t_to r; return ()
    recSizeOf r = sum [ sizeOf $ t_to_Type r, sizeOf $ t_to r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_to_Type r), viewField "" (viewNStr $ t_to r) ]
    recType = fromEnum . t_to_Type

instance Rec T_TS where
    recGet = do a01 <- get; a02 <- get; return $ T_TS a01 a02
    recPut r = do put $ t_ts_Type r; put $ t_ts r; return ()
    recSizeOf r = sum [ sizeOf $ t_ts_Type r, sizeOf $ t_ts r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ts_Type r), viewField "" (viewNStr $ t_ts r) ]
    recType = fromEnum . t_ts_Type

instance Rec T_UDTS where
    recGet = do a01 <- get; a02 <- get; return $ T_UDTS a01 a02
    recPut r = do put $ t_udts_Type r; put $ t_udts r; return ()
    recSizeOf r = sum [ sizeOf $ t_udts_Type r, sizeOf $ t_udts r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_udts_Type r), viewField "" (viewNStr $ t_udts r) ]
    recType = fromEnum . t_udts_Type

instance ChunkBuf T_ N1 Buffer1 where
    mkChunk = T_
    chunkDecon (T_ x y) = (x, y)
    chunkTypeLookup = lookupT
    chunkApply = applyT

applyT :: forall x. N1 -> T_ -> (forall a. (Rec a) => (a -> x)) -> x
applyT x rec f = case x of
    0x01 -> apply rec (f :: T_CGCSGI -> x)
    0x02 -> apply rec (f :: T_FQN -> x)
    0x04 -> apply rec (f :: T_MO -> x)
    0x10 -> apply rec (f :: T_OCL -> x)
    0x18 -> apply rec (f :: T_MIS -> x)
    0x1D -> apply rec (f :: T_TO -> x)
    0x1F -> apply rec (f :: T_FDS -> x)
    0x20 -> apply rec (f :: T_FCGCSGI -> x)
    0x21 -> apply rec (f :: T_OFSS -> x)
    0x22 -> apply rec (f :: T_ROT -> x)
    0x23 -> apply rec (f :: T_ERLI -> x)
    0x24 -> apply rec (f :: T_RLI -> x)
    0x25 -> apply rec (f :: T_RSN -> x)
    0x26 -> apply rec (f :: T_CR -> x)
    0x27 -> apply rec (f :: T_LDOPM -> x)
    0x2D -> apply rec (f :: T_OBO -> x)
    0x36 -> apply rec (f :: T_AV -> x)
    0x43 -> apply rec (f :: T_DP -> x)
    0x45 -> apply rec (f :: T_MEC -> x)
    0x46 -> apply rec (f :: T_POCP -> x)
    0x47 -> apply rec (f :: T_RUA -> x)
    0x4B -> apply rec (f :: T_OAMU -> x)
    0x4C -> apply rec (f :: T_OAS -> x)
    0x4D -> apply rec (f :: T_AD -> x)
    0x4E -> apply rec (f :: T_CS -> x)
    0x50 -> apply rec (f :: T_ESI -> x)
    0x56 -> apply rec (f :: T_MMPN -> x)
    0x57 -> apply rec (f :: T_OBE -> x)
    0x58 -> apply rec (f :: T_OSFO -> x)
    0x59 -> apply rec (f :: T_OSFE -> x)
    0x5A -> apply rec (f :: T_OO -> x)
    0x5D -> apply rec (f :: T_FHSF -> x)
    0x5E -> apply rec (f :: T_OCO -> x)
    0x62 -> apply rec (f :: T_LDTS -> x)
    0x63 -> apply rec (f :: T_T2FRMT -> x)
    0x64 -> apply rec (f :: T_OOI -> x)
    0x65 -> apply rec (f :: T_C -> x)
    0x68 -> apply rec (f :: T_MOR -> x)
    0x6C -> apply rec (f :: T_ROI -> x)
    0x6D -> apply rec (f :: T_EF -> x)
    0x70 -> apply rec (f :: T_PSRM -> x)
    0x71 -> apply rec (f :: T_PSMR -> x)
    0x72 -> apply rec (f :: T_UDTS -> x)
    0x73 -> apply rec (f :: T_II -> x)
    0x74 -> apply rec (f :: T_TS -> x)
    0x75 -> apply rec (f :: T_CF -> x)
    0x78 -> apply rec (f :: T_FF -> x)
    0x79 -> apply rec (f :: T_MA -> x)
    0x80 -> apply rec (f :: T_AQ -> x)
    0x81 -> apply rec (f :: T_PPI -> x)
    0x82 -> apply rec (f :: T_PV -> x)
    0x83 -> apply rec (f :: T_PC -> x)
    0x84 -> apply rec (f :: T_FRMT -> x)
    0x85 -> apply rec (f :: T_FO -> x)
    0x87 -> apply rec (f :: T_MF -> x)
    _    -> apply rec (f :: Unknown -> x)

