{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstancePTX () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals
import qualified Data.ByteString as S

apply :: (ChunkBuf c n b, Rec r) => c -> (r -> t) -> t
apply c f = f (decodeChunk c)

instance Rec PTX_AMB where
    recGet = do a01 <- get; a02 <- get; return $ PTX_AMB a01 a02
    recPut r = do put $ ptx_amb_Type r; put $ ptx_amb r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_amb_Type r, sizeOf $ ptx_amb r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_amb_Type r), viewField "" (viewNumber $ ptx_amb r) ]
    recType = fromEnum . ptx_amb_Type

instance Rec PTX_AMI where
    recGet = do a01 <- get; a02 <- get; return $ PTX_AMI a01 a02
    recPut r = do put $ ptx_ami_Type r; put $ ptx_ami r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_ami_Type r, sizeOf $ ptx_ami r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_ami_Type r), viewField "" (viewNumber $ ptx_ami r) ]
    recType = fromEnum . ptx_ami_Type

instance Rec PTX_BLN where
    recGet = do a01 <- get; a02 <- get; return $ PTX_BLN a01 a02
    recPut r = do put $ ptx_bln_Type r; put $ ptx_bln r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_bln_Type r, sizeOf $ ptx_bln r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_bln_Type r), viewField "" (viewNStr $ ptx_bln r) ]
    recType = fromEnum . ptx_bln_Type

instance Rec PTX_BSU where
    recGet = do a01 <- get; a02 <- get; return $ PTX_BSU a01 a02
    recPut r = do put $ ptx_bsu_Type r; put $ ptx_bsu r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_bsu_Type r, sizeOf $ ptx_bsu r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_bsu_Type r), viewField "" (viewNStr $ ptx_bsu r) ]
    recType = fromEnum . ptx_bsu_Type

instance Rec PTX_DBR where
    recGet = do a01 <- get; a02 <- get; return $ PTX_DBR a01 a02
    recPut r = do put $ ptx_dbr_Type r; put $ ptx_dbr r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_dbr_Type r, sizeOf $ ptx_dbr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_dbr_Type r), viewField "" (viewNStr $ ptx_dbr r) ]
    recType = fromEnum . ptx_dbr_Type

instance Rec PTX_DIR where
    recGet = do a01 <- get; a02 <- get; return $ PTX_DIR a01 a02
    recPut r = do put $ ptx_dir_Type r; put $ ptx_dir r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_dir_Type r, sizeOf $ ptx_dir r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_dir_Type r), viewField "" (viewNStr $ ptx_dir r) ]
    recType = fromEnum . ptx_dir_Type

instance Rec PTX_ESU where
    recGet = do a01 <- get; a02 <- get; return $ PTX_ESU a01 a02
    recPut r = do put $ ptx_esu_Type r; put $ ptx_esu r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_esu_Type r, sizeOf $ ptx_esu r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_esu_Type r), viewField "" (viewNStr $ ptx_esu r) ]
    recType = fromEnum . ptx_esu_Type

instance Rec PTX_NOP where
    recGet = do a01 <- get; a02 <- get; return $ PTX_NOP a01 a02
    recPut r = do put $ ptx_nop_Type r; put $ ptx_nop r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_nop_Type r, sizeOf $ ptx_nop r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_nop_Type r), viewField "" (viewNStr $ ptx_nop r) ]
    recType = fromEnum . ptx_nop_Type

instance Rec PTX_RMB where
    recGet = do a01 <- get; a02 <- get; return $ PTX_RMB a01 a02
    recPut r = do put $ ptx_rmb_Type r; put $ ptx_rmb r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_rmb_Type r, sizeOf $ ptx_rmb r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_rmb_Type r), viewField "" (viewNumber $ ptx_rmb r) ]
    recType = fromEnum . ptx_rmb_Type

instance Rec PTX_RMI where
    recGet = do a01 <- get; a02 <- get; return $ PTX_RMI a01 a02
    recPut r = do put $ ptx_rmi_Type r; put $ ptx_rmi r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_rmi_Type r, sizeOf $ ptx_rmi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_rmi_Type r), viewField "" (viewNumber $ ptx_rmi r) ]
    recType = fromEnum . ptx_rmi_Type

instance Rec PTX_RPS where
    recGet = do a01 <- get; a02 <- get; return $ PTX_RPS a01 a02
    recPut r = do put $ ptx_rps_Type r; put $ ptx_rps r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_rps_Type r, sizeOf $ ptx_rps r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_rps_Type r), viewField "" (viewNStr $ ptx_rps r) ]
    recType = fromEnum . ptx_rps_Type

instance Rec PTX_SBI where
    recGet = do a01 <- get; a02 <- get; return $ PTX_SBI a01 a02
    recPut r = do put $ ptx_sbi_Type r; put $ ptx_sbi r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_sbi_Type r, sizeOf $ ptx_sbi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_sbi_Type r), viewField "" (viewNumber $ ptx_sbi r) ]
    recType = fromEnum . ptx_sbi_Type

instance Rec PTX_SCFL where
    recGet = do a01 <- get; a02 <- get; return $ PTX_SCFL a01 a02
    recPut r = do put $ ptx_scfl_Type r; put $ ptx_scfl r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_scfl_Type r, sizeOf $ ptx_scfl r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_scfl_Type r), viewField "" (viewNumber $ ptx_scfl r) ]
    recType = fromEnum . ptx_scfl_Type

instance Rec PTX_SIA where
    recGet = do a01 <- get; a02 <- get; return $ PTX_SIA a01 a02
    recPut r = do put $ ptx_sia_Type r; put $ ptx_sia r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_sia_Type r, sizeOf $ ptx_sia r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_sia_Type r), viewField "" (viewNumber $ ptx_sia r) ]
    recType = fromEnum . ptx_sia_Type

instance Rec PTX_SIM where
    recGet = do a01 <- get; a02 <- get; return $ PTX_SIM a01 a02
    recPut r = do put $ ptx_sim_Type r; put $ ptx_sim r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_sim_Type r, sizeOf $ ptx_sim r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_sim_Type r), viewField "" (viewNumber $ ptx_sim r) ]
    recType = fromEnum . ptx_sim_Type

instance Rec PTX_STC where
    recGet = do a01 <- get; a02 <- get; return $ PTX_STC a01 a02
    recPut r = do put $ ptx_stc_Type r; put $ ptx_stc r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_stc_Type r, sizeOf $ ptx_stc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_stc_Type r), viewField "" (viewNStr $ ptx_stc r) ]
    recType = fromEnum . ptx_stc_Type

instance Rec PTX_STO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ PTX_STO a01 a02 a03
    recPut r = do put $ ptx_sto_Type r; put $ ptx_sto_Orientation r; put $ ptx_sto_WrapDirection r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_sto_Type r, sizeOf $ ptx_sto_Orientation r, sizeOf $ ptx_sto_WrapDirection r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_sto_Type r), viewField "Orientation" (viewNumber $ ptx_sto_Orientation r), viewField "WrapDirection" (viewNumber $ ptx_sto_WrapDirection r) ]
    recType = fromEnum . ptx_sto_Type

instance Rec PTX_SVI where
    recGet = do a01 <- get; a02 <- get; return $ PTX_SVI a01 a02
    recPut r = do put $ ptx_svi_Type r; put $ ptx_svi r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_svi_Type r, sizeOf $ ptx_svi r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_svi_Type r), viewField "" (viewNumber $ ptx_svi r) ]
    recType = fromEnum . ptx_svi_Type

instance Rec PTX_TRN where
    recGet = do a01 <- get; a02 <- get; return $ PTX_TRN a01 a02
    recPut r = do put $ ptx_trn_Type r; put $ ptx_trn r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_trn_Type r, sizeOf $ ptx_trn r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_trn_Type r), viewField "" (viewNStr $ ptx_trn r) ]
    recType = fromEnum . ptx_trn_Type

instance ChunkBuf PTX_ N1 Buffer1 where
    mkChunk = PTX_
    chunkDecon (PTX_ x y) = (x, y)
    chunkTypeLookup = lookupPTX
    chunkApply = applyPTX

applyPTX :: forall x. N1 -> PTX_ -> (forall a. (Rec a) => (a -> x)) -> x
applyPTX x rec f = case x of
    0x74 -> apply rec (f :: PTX_STC -> x)
    0x75 -> apply rec (f :: PTX_STC -> x)
    0xC0 -> apply rec (f :: PTX_SIM -> x)
    0xC1 -> apply rec (f :: PTX_SIM -> x)
    0xC2 -> apply rec (f :: PTX_SIA -> x)
    0xC3 -> apply rec (f :: PTX_SIA -> x)
    0xC4 -> apply rec (f :: PTX_SVI -> x)
    0xC5 -> apply rec (f :: PTX_SVI -> x)
    0xC6 -> apply rec (f :: PTX_AMI -> x)
    0xC7 -> apply rec (f :: PTX_AMI -> x)
    0xC8 -> apply rec (f :: PTX_RMI -> x)
    0xC9 -> apply rec (f :: PTX_RMI -> x)
    0xD0 -> apply rec (f :: PTX_SBI -> x)
    0xD1 -> apply rec (f :: PTX_SBI -> x)
    0xD2 -> apply rec (f :: PTX_AMB -> x)
    0xD3 -> apply rec (f :: PTX_AMB -> x)
    0xD4 -> apply rec (f :: PTX_RMB -> x)
    0xD5 -> apply rec (f :: PTX_RMB -> x)
    0xD8 -> apply rec (f :: PTX_BLN -> x)
    0xD9 -> apply rec (f :: PTX_BLN -> x)
    0xDA -> apply rec (f :: PTX_TRN -> x)
    0xDB -> apply rec (f :: PTX_TRN -> x)
    0xE4 -> apply rec (f :: PTX_DIR -> x)
    0xE5 -> apply rec (f :: PTX_DIR -> x)
    0xE6 -> apply rec (f :: PTX_DBR -> x)
    0xE7 -> apply rec (f :: PTX_DBR -> x)
    0xEE -> apply rec (f :: PTX_RPS -> x)
    0xEF -> apply rec (f :: PTX_RPS -> x)
    0xF0 -> apply rec (f :: PTX_SCFL -> x)
    0xF1 -> apply rec (f :: PTX_SCFL -> x)
    0xF2 -> apply rec (f :: PTX_BSU -> x)
    0xF3 -> apply rec (f :: PTX_BSU -> x)
    0xF4 -> apply rec (f :: PTX_ESU -> x)
    0xF5 -> apply rec (f :: PTX_ESU -> x)
    0xF6 -> apply rec (f :: PTX_STO -> x)
    0xF7 -> apply rec (f :: PTX_STO -> x)
    0xF8 -> apply rec (f :: PTX_NOP -> x)
    0xF9 -> apply rec (f :: PTX_NOP -> x)
    _    -> apply rec (f :: Unknown -> x)

instance RecChunk PTX PTX_ N1 Buffer1 where
    readChunks r = ptx_Chunks r
    writeChunks r io = do
        cs <- io
        return $ r { ptx_Chunks = cs }

instance Rec PTX_ where
    recSizeOf c = 1 + (S.length $ packChunk c)

instance Rec PTX where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- getList; return $ PTX a01 a02 a03 a04
    recPut r = do put $ ptx_Type r; put $ ptx_ r; put $ ptx_EscapeSequence r; putList $ ptx_Chunks r; return ()
    recSizeOf r = sum [ sizeOf $ ptx_Type r, sizeOf $ ptx_ r, sizeOf $ ptx_EscapeSequence r, sizeOf $ ptx_Chunks r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptx_Type r), viewField "_" (viewNumber $ ptx_ r), viewField "EscapeSequence" (viewNumber $ ptx_EscapeSequence r), viewField "Chunks" (viewChunks $ ptx_Chunks r) ]
    recType = fromEnum . ptx_Type

