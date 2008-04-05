{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstanceAFP.E () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

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
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ eii_Type r), viewField "_" (viewNumber $ eii_ r), viewField "ImageObjectName" (viewAStr $ eii_ImageObjectName r) ]
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
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ er_Type r), viewField "_" (viewNumber $ er_ r), viewField "" (viewAStr $ er r) ]
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

