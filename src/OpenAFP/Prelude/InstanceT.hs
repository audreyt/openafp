{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstanceT () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

recOf :: (ChunkBuf c n b, Binary (Record r)) => c -> IO r
recOf c = return . fromRecord =<< chunkToRecord c

apply :: (MonadIO m) => (t -> IO a) -> t -> (a -> m b) -> m b
apply ctr c f = f =<< (liftIO $ ctr c)

instance Rec T_AD where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_AD a01 a02
    recPut bh r = do put bh $ t_ad_Type r; put bh $ t_ad r; return ()
    recSizeOf r = sum [ sizeOf $ t_ad_Type r, sizeOf $ t_ad r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ad_Type r), viewField "" (viewNStr $ t_ad r) ]
    recType = fromEnum . t_ad_Type

instance Rec T_AQ where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_AQ a01 a02
    recPut bh r = do put bh $ t_aq_Type r; put bh $ t_aq r; return ()
    recSizeOf r = sum [ sizeOf $ t_aq_Type r, sizeOf $ t_aq r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_aq_Type r), viewField "" (viewNStr $ t_aq r) ]
    recType = fromEnum . t_aq_Type

instance Rec T_AV where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_AV a01 a02
    recPut bh r = do put bh $ t_av_Type r; put bh $ t_av r; return ()
    recSizeOf r = sum [ sizeOf $ t_av_Type r, sizeOf $ t_av r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_av_Type r), viewField "" (viewNStr $ t_av r) ]
    recType = fromEnum . t_av_Type

instance Rec T_C where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_C a01 a02
    recPut bh r = do put bh $ t_c_Type r; put bh $ t_c r; return ()
    recSizeOf r = sum [ sizeOf $ t_c_Type r, sizeOf $ t_c r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_c_Type r), viewField "" (viewNStr $ t_c r) ]
    recType = fromEnum . t_c_Type

instance Rec T_CF where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_CF a01 a02
    recPut bh r = do put bh $ t_cf_Type r; put bh $ t_cf r; return ()
    recSizeOf r = sum [ sizeOf $ t_cf_Type r, sizeOf $ t_cf r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_cf_Type r), viewField "" (viewNStr $ t_cf r) ]
    recType = fromEnum . t_cf_Type

instance Rec T_CGCSGI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_CGCSGI a01 a02
    recPut bh r = do put bh $ t_cgcsgi_Type r; put bh $ t_cgcsgi r; return ()
    recSizeOf r = sum [ sizeOf $ t_cgcsgi_Type r, sizeOf $ t_cgcsgi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_cgcsgi_Type r), viewField "" (viewNStr $ t_cgcsgi r) ]
    recType = fromEnum . t_cgcsgi_Type

instance Rec T_CR where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_CR a01 a02
    recPut bh r = do put bh $ t_cr_Type r; put bh $ t_cr r; return ()
    recSizeOf r = sum [ sizeOf $ t_cr_Type r, sizeOf $ t_cr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_cr_Type r), viewField "" (viewNStr $ t_cr r) ]
    recType = fromEnum . t_cr_Type

instance Rec T_CS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_CS a01 a02
    recPut bh r = do put bh $ t_cs_Type r; put bh $ t_cs r; return ()
    recSizeOf r = sum [ sizeOf $ t_cs_Type r, sizeOf $ t_cs r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_cs_Type r), viewField "" (viewNStr $ t_cs r) ]
    recType = fromEnum . t_cs_Type

instance Rec T_DP where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_DP a01 a02
    recPut bh r = do put bh $ t_dp_Type r; put bh $ t_dp r; return ()
    recSizeOf r = sum [ sizeOf $ t_dp_Type r, sizeOf $ t_dp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_dp_Type r), viewField "" (viewNStr $ t_dp r) ]
    recType = fromEnum . t_dp_Type

instance Rec T_EF where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_EF a01 a02
    recPut bh r = do put bh $ t_ef_Type r; put bh $ t_ef r; return ()
    recSizeOf r = sum [ sizeOf $ t_ef_Type r, sizeOf $ t_ef r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ef_Type r), viewField "" (viewNStr $ t_ef r) ]
    recType = fromEnum . t_ef_Type

instance Rec T_ERLI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_ERLI a01 a02
    recPut bh r = do put bh $ t_erli_Type r; put bh $ t_erli r; return ()
    recSizeOf r = sum [ sizeOf $ t_erli_Type r, sizeOf $ t_erli r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_erli_Type r), viewField "" (viewNStr $ t_erli r) ]
    recType = fromEnum . t_erli_Type

instance Rec T_ESI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_ESI a01 a02
    recPut bh r = do put bh $ t_esi_Type r; put bh $ t_esi r; return ()
    recSizeOf r = sum [ sizeOf $ t_esi_Type r, sizeOf $ t_esi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_esi_Type r), viewField "" (viewNStr $ t_esi r) ]
    recType = fromEnum . t_esi_Type

instance Rec T_FCGCSGI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_FCGCSGI a01 a02
    recPut bh r = do put bh $ t_fcgcsgi_Type r; put bh $ t_fcgcsgi r; return ()
    recSizeOf r = sum [ sizeOf $ t_fcgcsgi_Type r, sizeOf $ t_fcgcsgi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fcgcsgi_Type r), viewField "" (viewNStr $ t_fcgcsgi r) ]
    recType = fromEnum . t_fcgcsgi_Type

instance Rec T_FDS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_FDS a01 a02
    recPut bh r = do put bh $ t_fds_Type r; put bh $ t_fds r; return ()
    recSizeOf r = sum [ sizeOf $ t_fds_Type r, sizeOf $ t_fds r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fds_Type r), viewField "" (viewNStr $ t_fds r) ]
    recType = fromEnum . t_fds_Type

instance Rec T_FF where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_FF a01 a02
    recPut bh r = do put bh $ t_ff_Type r; put bh $ t_ff r; return ()
    recSizeOf r = sum [ sizeOf $ t_ff_Type r, sizeOf $ t_ff r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ff_Type r), viewField "" (viewNStr $ t_ff r) ]
    recType = fromEnum . t_ff_Type

instance Rec T_FHSF where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_FHSF a01 a02
    recPut bh r = do put bh $ t_fhsf_Type r; put bh $ t_fhsf r; return ()
    recSizeOf r = sum [ sizeOf $ t_fhsf_Type r, sizeOf $ t_fhsf r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fhsf_Type r), viewField "" (viewNStr $ t_fhsf r) ]
    recType = fromEnum . t_fhsf_Type

instance Rec T_FO where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_FO a01 a02
    recPut bh r = do put bh $ t_fo_Type r; put bh $ t_fo r; return ()
    recSizeOf r = sum [ sizeOf $ t_fo_Type r, sizeOf $ t_fo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fo_Type r), viewField "" (viewNStr $ t_fo r) ]
    recType = fromEnum . t_fo_Type

instance Rec T_FQN where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; a04 <- get bh; return $ T_FQN a01 a02 a03 a04
    recPut bh r = do put bh $ t_fqn_Type r; put bh $ t_fqn_SubType r; put bh $ t_fqn_Format r; put bh $ t_fqn r; return ()
    recSizeOf r = sum [ sizeOf $ t_fqn_Type r, sizeOf $ t_fqn_SubType r, sizeOf $ t_fqn_Format r, sizeOf $ t_fqn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_fqn_Type r), viewField "SubType" (viewNumber $ t_fqn_SubType r), viewField "Format" (viewNumber $ t_fqn_Format r), viewField "" (viewString $ t_fqn r) ]
    recType = fromEnum . t_fqn_Type

instance Rec T_FRMT where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_FRMT a01 a02
    recPut bh r = do put bh $ t_frmt_Type r; put bh $ t_frmt r; return ()
    recSizeOf r = sum [ sizeOf $ t_frmt_Type r, sizeOf $ t_frmt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_frmt_Type r), viewField "" (viewNStr $ t_frmt r) ]
    recType = fromEnum . t_frmt_Type

instance Rec T_II where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_II a01 a02
    recPut bh r = do put bh $ t_ii_Type r; put bh $ t_ii r; return ()
    recSizeOf r = sum [ sizeOf $ t_ii_Type r, sizeOf $ t_ii r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ii_Type r), viewField "" (viewNStr $ t_ii r) ]
    recType = fromEnum . t_ii_Type

instance Rec T_LDOPM where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_LDOPM a01 a02
    recPut bh r = do put bh $ t_ldopm_Type r; put bh $ t_ldopm r; return ()
    recSizeOf r = sum [ sizeOf $ t_ldopm_Type r, sizeOf $ t_ldopm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ldopm_Type r), viewField "" (viewNStr $ t_ldopm r) ]
    recType = fromEnum . t_ldopm_Type

instance Rec T_LDTS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_LDTS a01 a02
    recPut bh r = do put bh $ t_ldts_Type r; put bh $ t_ldts r; return ()
    recSizeOf r = sum [ sizeOf $ t_ldts_Type r, sizeOf $ t_ldts r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ldts_Type r), viewField "" (viewNStr $ t_ldts r) ]
    recType = fromEnum . t_ldts_Type

instance Rec T_MA where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_MA a01 a02
    recPut bh r = do put bh $ t_ma_Type r; put bh $ t_ma r; return ()
    recSizeOf r = sum [ sizeOf $ t_ma_Type r, sizeOf $ t_ma r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ma_Type r), viewField "" (viewNStr $ t_ma r) ]
    recType = fromEnum . t_ma_Type

instance Rec T_MEC where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_MEC a01 a02
    recPut bh r = do put bh $ t_mec_Type r; put bh $ t_mec r; return ()
    recSizeOf r = sum [ sizeOf $ t_mec_Type r, sizeOf $ t_mec r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mec_Type r), viewField "" (viewNStr $ t_mec r) ]
    recType = fromEnum . t_mec_Type

instance Rec T_MF where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_MF a01 a02
    recPut bh r = do put bh $ t_mf_Type r; put bh $ t_mf r; return ()
    recSizeOf r = sum [ sizeOf $ t_mf_Type r, sizeOf $ t_mf r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mf_Type r), viewField "" (viewNStr $ t_mf r) ]
    recType = fromEnum . t_mf_Type

instance Rec T_MIS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_MIS a01 a02
    recPut bh r = do put bh $ t_mis_Type r; put bh $ t_mis r; return ()
    recSizeOf r = sum [ sizeOf $ t_mis_Type r, sizeOf $ t_mis r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mis_Type r), viewField "" (viewNStr $ t_mis r) ]
    recType = fromEnum . t_mis_Type

instance Rec T_MMPN where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_MMPN a01 a02
    recPut bh r = do put bh $ t_mmpn_Type r; put bh $ t_mmpn r; return ()
    recSizeOf r = sum [ sizeOf $ t_mmpn_Type r, sizeOf $ t_mmpn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mmpn_Type r), viewField "" (viewNStr $ t_mmpn r) ]
    recType = fromEnum . t_mmpn_Type

instance Rec T_MO where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_MO a01 a02
    recPut bh r = do put bh $ t_mo_Type r; put bh $ t_mo r; return ()
    recSizeOf r = sum [ sizeOf $ t_mo_Type r, sizeOf $ t_mo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mo_Type r), viewField "" (viewNStr $ t_mo r) ]
    recType = fromEnum . t_mo_Type

instance Rec T_MOR where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_MOR a01 a02
    recPut bh r = do put bh $ t_mor_Type r; put bh $ t_mor r; return ()
    recSizeOf r = sum [ sizeOf $ t_mor_Type r, sizeOf $ t_mor r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_mor_Type r), viewField "" (viewNStr $ t_mor r) ]
    recType = fromEnum . t_mor_Type

instance Rec T_OAMU where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OAMU a01 a02
    recPut bh r = do put bh $ t_oamu_Type r; put bh $ t_oamu r; return ()
    recSizeOf r = sum [ sizeOf $ t_oamu_Type r, sizeOf $ t_oamu r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_oamu_Type r), viewField "" (viewNStr $ t_oamu r) ]
    recType = fromEnum . t_oamu_Type

instance Rec T_OAS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OAS a01 a02
    recPut bh r = do put bh $ t_oas_Type r; put bh $ t_oas r; return ()
    recSizeOf r = sum [ sizeOf $ t_oas_Type r, sizeOf $ t_oas r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_oas_Type r), viewField "" (viewNStr $ t_oas r) ]
    recType = fromEnum . t_oas_Type

instance Rec T_OBE where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OBE a01 a02
    recPut bh r = do put bh $ t_obe_Type r; put bh $ t_obe r; return ()
    recSizeOf r = sum [ sizeOf $ t_obe_Type r, sizeOf $ t_obe r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_obe_Type r), viewField "" (viewNStr $ t_obe r) ]
    recType = fromEnum . t_obe_Type

instance Rec T_OBO where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OBO a01 a02
    recPut bh r = do put bh $ t_obo_Type r; put bh $ t_obo r; return ()
    recSizeOf r = sum [ sizeOf $ t_obo_Type r, sizeOf $ t_obo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_obo_Type r), viewField "" (viewNStr $ t_obo r) ]
    recType = fromEnum . t_obo_Type

instance Rec T_OCH where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OCH a01 a02
    recPut bh r = do put bh $ t_och_Type r; put bh $ t_och r; return ()
    recSizeOf r = sum [ sizeOf $ t_och_Type r, sizeOf $ t_och r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_och_Type r), viewField "" (viewNStr $ t_och r) ]
    recType = fromEnum . t_och_Type

instance Rec T_OCL where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OCL a01 a02
    recPut bh r = do put bh $ t_ocl_Type r; put bh $ t_ocl r; return ()
    recSizeOf r = sum [ sizeOf $ t_ocl_Type r, sizeOf $ t_ocl r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ocl_Type r), viewField "" (viewNStr $ t_ocl r) ]
    recType = fromEnum . t_ocl_Type

instance Rec T_OCO where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OCO a01 a02
    recPut bh r = do put bh $ t_oco_Type r; put bh $ t_oco r; return ()
    recSizeOf r = sum [ sizeOf $ t_oco_Type r, sizeOf $ t_oco r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_oco_Type r), viewField "" (viewNStr $ t_oco r) ]
    recType = fromEnum . t_oco_Type

instance Rec T_OFSS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OFSS a01 a02
    recPut bh r = do put bh $ t_ofss_Type r; put bh $ t_ofss r; return ()
    recSizeOf r = sum [ sizeOf $ t_ofss_Type r, sizeOf $ t_ofss r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ofss_Type r), viewField "" (viewNStr $ t_ofss r) ]
    recType = fromEnum . t_ofss_Type

instance Rec T_OO where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OO a01 a02
    recPut bh r = do put bh $ t_oo_Type r; put bh $ t_oo r; return ()
    recSizeOf r = sum [ sizeOf $ t_oo_Type r, sizeOf $ t_oo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_oo_Type r), viewField "" (viewNStr $ t_oo r) ]
    recType = fromEnum . t_oo_Type

instance Rec T_OOI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OOI a01 a02
    recPut bh r = do put bh $ t_ooi_Type r; put bh $ t_ooi r; return ()
    recSizeOf r = sum [ sizeOf $ t_ooi_Type r, sizeOf $ t_ooi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ooi_Type r), viewField "" (viewNStr $ t_ooi r) ]
    recType = fromEnum . t_ooi_Type

instance Rec T_OSFE where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OSFE a01 a02
    recPut bh r = do put bh $ t_osfe_Type r; put bh $ t_osfe r; return ()
    recSizeOf r = sum [ sizeOf $ t_osfe_Type r, sizeOf $ t_osfe r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_osfe_Type r), viewField "" (viewNStr $ t_osfe r) ]
    recType = fromEnum . t_osfe_Type

instance Rec T_OSFO where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_OSFO a01 a02
    recPut bh r = do put bh $ t_osfo_Type r; put bh $ t_osfo r; return ()
    recSizeOf r = sum [ sizeOf $ t_osfo_Type r, sizeOf $ t_osfo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_osfo_Type r), viewField "" (viewNStr $ t_osfo r) ]
    recType = fromEnum . t_osfo_Type

instance Rec T_PC where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_PC a01 a02
    recPut bh r = do put bh $ t_pc_Type r; put bh $ t_pc r; return ()
    recSizeOf r = sum [ sizeOf $ t_pc_Type r, sizeOf $ t_pc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_pc_Type r), viewField "" (viewNStr $ t_pc r) ]
    recType = fromEnum . t_pc_Type

instance Rec T_POCP where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_POCP a01 a02
    recPut bh r = do put bh $ t_pocp_Type r; put bh $ t_pocp r; return ()
    recSizeOf r = sum [ sizeOf $ t_pocp_Type r, sizeOf $ t_pocp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_pocp_Type r), viewField "" (viewNStr $ t_pocp r) ]
    recType = fromEnum . t_pocp_Type

instance Rec T_PPI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_PPI a01 a02
    recPut bh r = do put bh $ t_ppi_Type r; put bh $ t_ppi r; return ()
    recSizeOf r = sum [ sizeOf $ t_ppi_Type r, sizeOf $ t_ppi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ppi_Type r), viewField "" (viewNStr $ t_ppi r) ]
    recType = fromEnum . t_ppi_Type

instance Rec T_PSMR where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_PSMR a01 a02
    recPut bh r = do put bh $ t_psmr_Type r; put bh $ t_psmr r; return ()
    recSizeOf r = sum [ sizeOf $ t_psmr_Type r, sizeOf $ t_psmr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_psmr_Type r), viewField "" (viewNStr $ t_psmr r) ]
    recType = fromEnum . t_psmr_Type

instance Rec T_PSRM where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_PSRM a01 a02
    recPut bh r = do put bh $ t_psrm_Type r; put bh $ t_psrm r; return ()
    recSizeOf r = sum [ sizeOf $ t_psrm_Type r, sizeOf $ t_psrm r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_psrm_Type r), viewField "" (viewNStr $ t_psrm r) ]
    recType = fromEnum . t_psrm_Type

instance Rec T_PV where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_PV a01 a02
    recPut bh r = do put bh $ t_pv_Type r; put bh $ t_pv r; return ()
    recSizeOf r = sum [ sizeOf $ t_pv_Type r, sizeOf $ t_pv r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_pv_Type r), viewField "" (viewNStr $ t_pv r) ]
    recType = fromEnum . t_pv_Type

instance Rec T_RLI where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ T_RLI a01 a02 a03
    recPut bh r = do put bh $ t_rli_Type r; put bh $ t_rli_SubType r; put bh $ t_rli r; return ()
    recSizeOf r = sum [ sizeOf $ t_rli_Type r, sizeOf $ t_rli_SubType r, sizeOf $ t_rli r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_rli_Type r), viewField "SubType" (viewNumber $ t_rli_SubType r), viewField "" (viewNumber $ t_rli r) ]
    recType = fromEnum . t_rli_Type

instance Rec T_ROI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_ROI a01 a02
    recPut bh r = do put bh $ t_roi_Type r; put bh $ t_roi r; return ()
    recSizeOf r = sum [ sizeOf $ t_roi_Type r, sizeOf $ t_roi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_roi_Type r), viewField "" (viewNStr $ t_roi r) ]
    recType = fromEnum . t_roi_Type

instance Rec T_ROT where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_ROT a01 a02
    recPut bh r = do put bh $ t_rot_Type r; put bh $ t_rot r; return ()
    recSizeOf r = sum [ sizeOf $ t_rot_Type r, sizeOf $ t_rot r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_rot_Type r), viewField "" (viewNStr $ t_rot r) ]
    recType = fromEnum . t_rot_Type

instance Rec T_RSN where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_RSN a01 a02
    recPut bh r = do put bh $ t_rsn_Type r; put bh $ t_rsn r; return ()
    recSizeOf r = sum [ sizeOf $ t_rsn_Type r, sizeOf $ t_rsn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_rsn_Type r), viewField "" (viewNStr $ t_rsn r) ]
    recType = fromEnum . t_rsn_Type

instance Rec T_RUA where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_RUA a01 a02
    recPut bh r = do put bh $ t_rua_Type r; put bh $ t_rua r; return ()
    recSizeOf r = sum [ sizeOf $ t_rua_Type r, sizeOf $ t_rua r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_rua_Type r), viewField "" (viewNStr $ t_rua r) ]
    recType = fromEnum . t_rua_Type

instance Rec T_T1CRMT where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_T1CRMT a01 a02
    recPut bh r = do put bh $ t_t1crmt_Type r; put bh $ t_t1crmt r; return ()
    recSizeOf r = sum [ sizeOf $ t_t1crmt_Type r, sizeOf $ t_t1crmt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_t1crmt_Type r), viewField "" (viewNStr $ t_t1crmt r) ]
    recType = fromEnum . t_t1crmt_Type

instance Rec T_T2FRMT where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_T2FRMT a01 a02
    recPut bh r = do put bh $ t_t2frmt_Type r; put bh $ t_t2frmt r; return ()
    recSizeOf r = sum [ sizeOf $ t_t2frmt_Type r, sizeOf $ t_t2frmt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_t2frmt_Type r), viewField "" (viewNStr $ t_t2frmt r) ]
    recType = fromEnum . t_t2frmt_Type

instance Rec T_TO where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_TO a01 a02
    recPut bh r = do put bh $ t_to_Type r; put bh $ t_to r; return ()
    recSizeOf r = sum [ sizeOf $ t_to_Type r, sizeOf $ t_to r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_to_Type r), viewField "" (viewNStr $ t_to r) ]
    recType = fromEnum . t_to_Type

instance Rec T_TS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_TS a01 a02
    recPut bh r = do put bh $ t_ts_Type r; put bh $ t_ts r; return ()
    recSizeOf r = sum [ sizeOf $ t_ts_Type r, sizeOf $ t_ts r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_ts_Type r), viewField "" (viewNStr $ t_ts r) ]
    recType = fromEnum . t_ts_Type

instance Rec T_UDTS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ T_UDTS a01 a02
    recPut bh r = do put bh $ t_udts_Type r; put bh $ t_udts r; return ()
    recSizeOf r = sum [ sizeOf $ t_udts_Type r, sizeOf $ t_udts r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ t_udts_Type r), viewField "" (viewNStr $ t_udts r) ]
    recType = fromEnum . t_udts_Type

instance ChunkBuf T_ N1 Buffer1 where
    chunkCon = T_
    chunkDecon (T_ x) = x
    chunkTypeLookup = lookupT
    chunkApply = applyT

applyT :: (MonadIO m) => N1 -> T_ -> (forall a. (Rec a) => (a -> m b)) -> m b
applyT x = case x of
    0x01 -> apply (recOf :: T_ -> IO T_CGCSGI)
    0x02 -> apply (recOf :: T_ -> IO T_FQN)
    0x04 -> apply (recOf :: T_ -> IO T_MO)
    0x10 -> apply (recOf :: T_ -> IO T_OCL)
    0x18 -> apply (recOf :: T_ -> IO T_MIS)
    0x1D -> apply (recOf :: T_ -> IO T_TO)
    0x1F -> apply (recOf :: T_ -> IO T_FDS)
    0x20 -> apply (recOf :: T_ -> IO T_FCGCSGI)
    0x21 -> apply (recOf :: T_ -> IO T_OFSS)
    0x22 -> apply (recOf :: T_ -> IO T_ROT)
    0x23 -> apply (recOf :: T_ -> IO T_ERLI)
    0x24 -> apply (recOf :: T_ -> IO T_RLI)
    0x25 -> apply (recOf :: T_ -> IO T_RSN)
    0x26 -> apply (recOf :: T_ -> IO T_CR)
    0x27 -> apply (recOf :: T_ -> IO T_LDOPM)
    0x2D -> apply (recOf :: T_ -> IO T_OBO)
    0x36 -> apply (recOf :: T_ -> IO T_AV)
    0x43 -> apply (recOf :: T_ -> IO T_DP)
    0x45 -> apply (recOf :: T_ -> IO T_MEC)
    0x46 -> apply (recOf :: T_ -> IO T_POCP)
    0x47 -> apply (recOf :: T_ -> IO T_RUA)
    0x4B -> apply (recOf :: T_ -> IO T_OAMU)
    0x4C -> apply (recOf :: T_ -> IO T_OAS)
    0x4D -> apply (recOf :: T_ -> IO T_AD)
    0x4E -> apply (recOf :: T_ -> IO T_CS)
    0x50 -> apply (recOf :: T_ -> IO T_ESI)
    0x56 -> apply (recOf :: T_ -> IO T_MMPN)
    0x57 -> apply (recOf :: T_ -> IO T_OBE)
    0x58 -> apply (recOf :: T_ -> IO T_OSFO)
    0x59 -> apply (recOf :: T_ -> IO T_OSFE)
    0x5A -> apply (recOf :: T_ -> IO T_OO)
    0x5D -> apply (recOf :: T_ -> IO T_FHSF)
    0x5E -> apply (recOf :: T_ -> IO T_OCO)
    0x62 -> apply (recOf :: T_ -> IO T_LDTS)
    0x63 -> apply (recOf :: T_ -> IO T_T2FRMT)
    0x64 -> apply (recOf :: T_ -> IO T_OOI)
    0x65 -> apply (recOf :: T_ -> IO T_C)
    0x68 -> apply (recOf :: T_ -> IO T_MOR)
    0x6C -> apply (recOf :: T_ -> IO T_ROI)
    0x6D -> apply (recOf :: T_ -> IO T_EF)
    0x70 -> apply (recOf :: T_ -> IO T_PSRM)
    0x71 -> apply (recOf :: T_ -> IO T_PSMR)
    0x72 -> apply (recOf :: T_ -> IO T_UDTS)
    0x73 -> apply (recOf :: T_ -> IO T_II)
    0x74 -> apply (recOf :: T_ -> IO T_TS)
    0x75 -> apply (recOf :: T_ -> IO T_CF)
    0x78 -> apply (recOf :: T_ -> IO T_FF)
    0x79 -> apply (recOf :: T_ -> IO T_MA)
    0x80 -> apply (recOf :: T_ -> IO T_AQ)
    0x81 -> apply (recOf :: T_ -> IO T_PPI)
    0x82 -> apply (recOf :: T_ -> IO T_PV)
    0x83 -> apply (recOf :: T_ -> IO T_PC)
    0x84 -> apply (recOf :: T_ -> IO T_FRMT)
    0x85 -> apply (recOf :: T_ -> IO T_FO)
    0x87 -> apply (recOf :: T_ -> IO T_MF)
    _    -> apply (recOf :: T_ -> IO Unknown)

