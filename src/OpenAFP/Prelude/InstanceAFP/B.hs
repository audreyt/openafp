{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstanceAFP.B () where
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
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ bii_Type r), viewField "_" (viewNumber $ bii_ r), viewField "ImageObjectName" (viewAStr $ bii_ImageObjectName r) ]
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

