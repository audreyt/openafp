{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstancePTX () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

recOf :: (ChunkBuf c n b, Binary (Record r)) => c -> IO r
recOf c = return . fromRecord =<< chunkToRecord c

apply :: (MonadIO m) => (t -> IO a) -> t -> (a -> m b) -> m b
apply ctr c f = f =<< (liftIO $ ctr c)

instance Rec PTX_AMB where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_AMB a01 a02
    recPut bh r = do put bh $ ptx_amb_Type r; put bh $ ptx_amb r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_amb_Type r, sizeOf $ ptx_amb r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_amb_Type r), viewField "" (viewNumber $ ptx_amb r) ]
    recType = fromEnum . ptx_amb_Type

instance Rec PTX_AMI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_AMI a01 a02
    recPut bh r = do put bh $ ptx_ami_Type r; put bh $ ptx_ami r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_ami_Type r, sizeOf $ ptx_ami r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_ami_Type r), viewField "" (viewNumber $ ptx_ami r) ]
    recType = fromEnum . ptx_ami_Type

instance Rec PTX_BLN where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_BLN a01 a02
    recPut bh r = do put bh $ ptx_bln_Type r; put bh $ ptx_bln r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_bln_Type r, sizeOf $ ptx_bln r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_bln_Type r), viewField "" (viewNStr $ ptx_bln r) ]
    recType = fromEnum . ptx_bln_Type

instance Rec PTX_BSU where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_BSU a01 a02
    recPut bh r = do put bh $ ptx_bsu_Type r; put bh $ ptx_bsu r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_bsu_Type r, sizeOf $ ptx_bsu r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_bsu_Type r), viewField "" (viewNStr $ ptx_bsu r) ]
    recType = fromEnum . ptx_bsu_Type

instance Rec PTX_DBR where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_DBR a01 a02
    recPut bh r = do put bh $ ptx_dbr_Type r; put bh $ ptx_dbr r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_dbr_Type r, sizeOf $ ptx_dbr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_dbr_Type r), viewField "" (viewNStr $ ptx_dbr r) ]
    recType = fromEnum . ptx_dbr_Type

instance Rec PTX_DIR where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_DIR a01 a02
    recPut bh r = do put bh $ ptx_dir_Type r; put bh $ ptx_dir r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_dir_Type r, sizeOf $ ptx_dir r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_dir_Type r), viewField "" (viewNStr $ ptx_dir r) ]
    recType = fromEnum . ptx_dir_Type

instance Rec PTX_ESU where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_ESU a01 a02
    recPut bh r = do put bh $ ptx_esu_Type r; put bh $ ptx_esu r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_esu_Type r, sizeOf $ ptx_esu r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_esu_Type r), viewField "" (viewNStr $ ptx_esu r) ]
    recType = fromEnum . ptx_esu_Type

instance Rec PTX_NOP where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_NOP a01 a02
    recPut bh r = do put bh $ ptx_nop_Type r; put bh $ ptx_nop r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_nop_Type r, sizeOf $ ptx_nop r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_nop_Type r), viewField "" (viewNStr $ ptx_nop r) ]
    recType = fromEnum . ptx_nop_Type

instance Rec PTX_RMB where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_RMB a01 a02
    recPut bh r = do put bh $ ptx_rmb_Type r; put bh $ ptx_rmb r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_rmb_Type r, sizeOf $ ptx_rmb r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_rmb_Type r), viewField "" (viewNumber $ ptx_rmb r) ]
    recType = fromEnum . ptx_rmb_Type

instance Rec PTX_RMI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_RMI a01 a02
    recPut bh r = do put bh $ ptx_rmi_Type r; put bh $ ptx_rmi r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_rmi_Type r, sizeOf $ ptx_rmi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_rmi_Type r), viewField "" (viewNumber $ ptx_rmi r) ]
    recType = fromEnum . ptx_rmi_Type

instance Rec PTX_RPS where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_RPS a01 a02
    recPut bh r = do put bh $ ptx_rps_Type r; put bh $ ptx_rps r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_rps_Type r, sizeOf $ ptx_rps r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_rps_Type r), viewField "" (viewNStr $ ptx_rps r) ]
    recType = fromEnum . ptx_rps_Type

instance Rec PTX_SBI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_SBI a01 a02
    recPut bh r = do put bh $ ptx_sbi_Type r; put bh $ ptx_sbi r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_sbi_Type r, sizeOf $ ptx_sbi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_sbi_Type r), viewField "" (viewNumber $ ptx_sbi r) ]
    recType = fromEnum . ptx_sbi_Type

instance Rec PTX_SCFL where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_SCFL a01 a02
    recPut bh r = do put bh $ ptx_scfl_Type r; put bh $ ptx_scfl r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_scfl_Type r, sizeOf $ ptx_scfl r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_scfl_Type r), viewField "" (viewNumber $ ptx_scfl r) ]
    recType = fromEnum . ptx_scfl_Type

instance Rec PTX_SIA where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_SIA a01 a02
    recPut bh r = do put bh $ ptx_sia_Type r; put bh $ ptx_sia r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_sia_Type r, sizeOf $ ptx_sia r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_sia_Type r), viewField "" (viewNumber $ ptx_sia r) ]
    recType = fromEnum . ptx_sia_Type

instance Rec PTX_SIM where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_SIM a01 a02
    recPut bh r = do put bh $ ptx_sim_Type r; put bh $ ptx_sim r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_sim_Type r, sizeOf $ ptx_sim r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_sim_Type r), viewField "" (viewNumber $ ptx_sim r) ]
    recType = fromEnum . ptx_sim_Type

instance Rec PTX_STC where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_STC a01 a02
    recPut bh r = do put bh $ ptx_stc_Type r; put bh $ ptx_stc r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_stc_Type r, sizeOf $ ptx_stc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_stc_Type r), viewField "" (viewNStr $ ptx_stc r) ]
    recType = fromEnum . ptx_stc_Type

instance Rec PTX_STO where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ PTX_STO a01 a02 a03
    recPut bh r = do put bh $ ptx_sto_Type r; put bh $ ptx_sto_Orientation r; put bh $ ptx_sto_WrapDirection r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_sto_Type r, sizeOf $ ptx_sto_Orientation r, sizeOf $ ptx_sto_WrapDirection r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_sto_Type r), viewField "Orientation" (viewNumber $ ptx_sto_Orientation r), viewField "WrapDirection" (viewNumber $ ptx_sto_WrapDirection r) ]
    recType = fromEnum . ptx_sto_Type

instance Rec PTX_SVI where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_SVI a01 a02
    recPut bh r = do put bh $ ptx_svi_Type r; put bh $ ptx_svi r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_svi_Type r, sizeOf $ ptx_svi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_svi_Type r), viewField "" (viewNumber $ ptx_svi r) ]
    recType = fromEnum . ptx_svi_Type

instance Rec PTX_TRN where
    recGet bh = do a01 <- get bh; a02 <- get bh; return $ PTX_TRN a01 a02
    recPut bh r = do put bh $ ptx_trn_Type r; put bh $ ptx_trn r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_trn_Type r, sizeOf $ ptx_trn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_trn_Type r), viewField "" (viewNStr $ ptx_trn r) ]
    recType = fromEnum . ptx_trn_Type

instance ChunkBuf PTX_ N1 Buffer1 where
    chunkCon = PTX_
    chunkDecon (PTX_ x) = x
    chunkTypeLookup = lookupPTX
    chunkApply = applyPTX

applyPTX :: (MonadIO m) => N1 -> PTX_ -> (forall a. (Rec a) => (a -> m b)) -> m b
applyPTX x = case x of
    0x74 -> apply (recOf :: PTX_ -> IO PTX_STC)
    0x75 -> apply (recOf :: PTX_ -> IO PTX_STC)
    0xC0 -> apply (recOf :: PTX_ -> IO PTX_SIM)
    0xC1 -> apply (recOf :: PTX_ -> IO PTX_SIM)
    0xC2 -> apply (recOf :: PTX_ -> IO PTX_SIA)
    0xC3 -> apply (recOf :: PTX_ -> IO PTX_SIA)
    0xC4 -> apply (recOf :: PTX_ -> IO PTX_SVI)
    0xC5 -> apply (recOf :: PTX_ -> IO PTX_SVI)
    0xC6 -> apply (recOf :: PTX_ -> IO PTX_AMI)
    0xC7 -> apply (recOf :: PTX_ -> IO PTX_AMI)
    0xC8 -> apply (recOf :: PTX_ -> IO PTX_RMI)
    0xC9 -> apply (recOf :: PTX_ -> IO PTX_RMI)
    0xD0 -> apply (recOf :: PTX_ -> IO PTX_SBI)
    0xD1 -> apply (recOf :: PTX_ -> IO PTX_SBI)
    0xD2 -> apply (recOf :: PTX_ -> IO PTX_AMB)
    0xD3 -> apply (recOf :: PTX_ -> IO PTX_AMB)
    0xD4 -> apply (recOf :: PTX_ -> IO PTX_RMB)
    0xD5 -> apply (recOf :: PTX_ -> IO PTX_RMB)
    0xD8 -> apply (recOf :: PTX_ -> IO PTX_BLN)
    0xD9 -> apply (recOf :: PTX_ -> IO PTX_BLN)
    0xDA -> apply (recOf :: PTX_ -> IO PTX_TRN)
    0xDB -> apply (recOf :: PTX_ -> IO PTX_TRN)
    0xE4 -> apply (recOf :: PTX_ -> IO PTX_DIR)
    0xE5 -> apply (recOf :: PTX_ -> IO PTX_DIR)
    0xE6 -> apply (recOf :: PTX_ -> IO PTX_DBR)
    0xE7 -> apply (recOf :: PTX_ -> IO PTX_DBR)
    0xEE -> apply (recOf :: PTX_ -> IO PTX_RPS)
    0xEF -> apply (recOf :: PTX_ -> IO PTX_RPS)
    0xF0 -> apply (recOf :: PTX_ -> IO PTX_SCFL)
    0xF1 -> apply (recOf :: PTX_ -> IO PTX_SCFL)
    0xF2 -> apply (recOf :: PTX_ -> IO PTX_BSU)
    0xF3 -> apply (recOf :: PTX_ -> IO PTX_BSU)
    0xF4 -> apply (recOf :: PTX_ -> IO PTX_ESU)
    0xF5 -> apply (recOf :: PTX_ -> IO PTX_ESU)
    0xF6 -> apply (recOf :: PTX_ -> IO PTX_STO)
    0xF7 -> apply (recOf :: PTX_ -> IO PTX_STO)
    0xF8 -> apply (recOf :: PTX_ -> IO PTX_NOP)
    0xF9 -> apply (recOf :: PTX_ -> IO PTX_NOP)
    _    -> apply (recOf :: PTX_ -> IO Unknown)

instance RecChunk PTX PTX_ N1 Buffer1 where
    readChunks r = ptx_Chunks r
    writeChunks r io = do
        cs <- io
        return $ r { ptx_Chunks = cs }

instance Rec PTX_ where
    recSizeOf c = 1 + (snd $ chunkToPStrLen c)

instance Rec PTX where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; a04 <- get bh; return $ PTX a01 a02 a03 a04
    recPut bh r = do put bh $ ptx_Type r; put bh $ ptx_ r; put bh $ ptx_EscapeSequence r; put bh $ ptx_Chunks r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_Type r, sizeOf $ ptx_ r, sizeOf $ ptx_EscapeSequence r, sizeOf $ ptx_Chunks r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_Type r), viewField "_" (viewNumber $ ptx_ r), viewField "EscapeSequence" (viewNumber $ ptx_EscapeSequence r), viewField "Chunks" (viewChunks $ ptx_Chunks r) ]
    recType = fromEnum . ptx_Type

