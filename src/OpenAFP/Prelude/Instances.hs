{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.Instances where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals
import OpenAFP.Prelude.InstanceT
import OpenAFP.Prelude.InstancePTX
import OpenAFP.Prelude.InstanceAFP

instance Rec T_ where
    recSizeOf c = 1 + (snd $ chunkToPStrLen c)

instance ChunkBuf AFP_ N3 Buffer2 where
    chunkCon = AFP_
    chunkDecon (AFP_ x) = x
    chunkTypeLookup = lookupAFP
    chunkApply = applyAFP

recOf :: (ChunkBuf c n b, Binary (Record r)) => c -> IO r
recOf c = return . fromRecord =<< chunkToRecord c

apply :: (MonadIO m) => (t -> IO a) -> t -> (a -> m b) -> m b
apply ctr c f = f =<< (liftIO $ ctr c)

applyAFP :: forall m x. (MonadIO m)
    => N3 -> AFP_ -> (forall a. (Rec a) => (a -> m x)) -> m x
applyAFP x rec f = case x of
    0xD38C87 -> apply recOf rec (f :: CPI -> m x)
    0xD38C89 -> apply recOf rec (f :: FNI -> m x)
    0xD38C8A -> apply recOf rec (f :: CFI -> m x)
    0xD3A088 -> apply recOf rec (f :: MFC -> m x)
    0xD3A090 -> apply recOf rec (f :: TLE -> m x)
    0xD3A288 -> apply recOf rec (f :: MCC -> m x)
    0xD3A289 -> apply recOf rec (f :: FNM -> m x)
    0xD3A66B -> apply recOf rec (f :: OBD -> m x)
    0xD3A67B -> apply recOf rec (f :: IID -> m x)
    0xD3A687 -> apply recOf rec (f :: CPD -> m x)
    0xD3A688 -> apply recOf rec (f :: MDD -> m x)
    0xD3A689 -> apply recOf rec (f :: FND -> m x)
    0xD3A692 -> apply recOf rec (f :: CDD -> m x)
    0xD3A69B -> apply recOf rec (f :: PTD1 -> m x)
    0xD3A6AF -> apply recOf rec (f :: PGD -> m x)
    0xD3A6BB -> apply recOf rec (f :: GDD -> m x)
    0xD3A6C5 -> apply recOf rec (f :: FGD -> m x)
    0xD3A6E3 -> apply recOf rec (f :: DXD -> m x)
    0xD3A6E7 -> apply recOf rec (f :: LND -> m x)
    0xD3A6EB -> apply recOf rec (f :: BDD -> m x)
    0xD3A6FB -> apply recOf rec (f :: IDD -> m x)
    0xD3A77B -> apply recOf rec (f :: IOC -> m x)
    0xD3A787 -> apply recOf rec (f :: CPC -> m x)
    0xD3A788 -> apply recOf rec (f :: MMC -> m x)
    0xD3A789 -> apply recOf rec (f :: FNC -> m x)
    0xD3A78A -> apply recOf rec (f :: CFC -> m x)
    0xD3A79B -> apply recOf rec (f :: CTC -> m x)
    0xD3A7AF -> apply recOf rec (f :: PMC -> m x)
    0xD3A85F -> apply recOf rec (f :: BPS -> m x)
    0xD3A877 -> apply recOf rec (f :: BCA -> m x)
    0xD3A87B -> apply recOf rec (f :: BII -> m x)
    0xD3A887 -> apply recOf rec (f :: BCP -> m x)
    0xD3A889 -> apply recOf rec (f :: BFN -> m x)
    0xD3A88A -> apply recOf rec (f :: BCF -> m x)
    0xD3A892 -> apply recOf rec (f :: BOC -> m x)
    0xD3A89B -> apply recOf rec (f :: BPT -> m x)
    0xD3A8A7 -> apply recOf rec (f :: BDI -> m x)
    0xD3A8A8 -> apply recOf rec (f :: BDT -> m x)
    0xD3A8AD -> apply recOf rec (f :: BNG -> m x)
    0xD3A8AF -> apply recOf rec (f :: BPG -> m x)
    0xD3A8BB -> apply recOf rec (f :: BGR -> m x)
    0xD3A8C4 -> apply recOf rec (f :: BDG -> m x)
    0xD3A8C5 -> apply recOf rec (f :: BFG -> m x)
    0xD3A8C6 -> apply recOf rec (f :: BRG -> m x)
    0xD3A8C7 -> apply recOf rec (f :: BOG -> m x)
    0xD3A8C9 -> apply recOf rec (f :: BAG -> m x)
    0xD3A8CA -> apply recOf rec (f :: BDM -> m x)
    0xD3A8CB -> apply recOf rec (f :: BPM -> m x)
    0xD3A8CC -> apply recOf rec (f :: BMM -> m x)
    0xD3A8CD -> apply recOf rec (f :: BFM -> m x)
    0xD3A8CE -> apply recOf rec (f :: BR -> m x)
    0xD3A8D9 -> apply recOf rec (f :: BSG -> m x)
    0xD3A8DF -> apply recOf rec (f :: BMO -> m x)
    0xD3A8E3 -> apply recOf rec (f :: BDX -> m x)
    0xD3A8EB -> apply recOf rec (f :: BBC -> m x)
    0xD3A8FB -> apply recOf rec (f :: BIM -> m x)
    0xD3A95F -> apply recOf rec (f :: EPS -> m x)
    0xD3A977 -> apply recOf rec (f :: ECA -> m x)
    0xD3A97B -> apply recOf rec (f :: EII -> m x)
    0xD3A987 -> apply recOf rec (f :: ECP -> m x)
    0xD3A989 -> apply recOf rec (f :: EFN -> m x)
    0xD3A98A -> apply recOf rec (f :: ECF -> m x)
    0xD3A992 -> apply recOf rec (f :: EOC -> m x)
    0xD3A99B -> apply recOf rec (f :: EPT -> m x)
    0xD3A9A7 -> apply recOf rec (f :: EDI -> m x)
    0xD3A9A8 -> apply recOf rec (f :: EDT -> m x)
    0xD3A9AD -> apply recOf rec (f :: ENG -> m x)
    0xD3A9AF -> apply recOf rec (f :: EPG -> m x)
    0xD3A9BB -> apply recOf rec (f :: EGR -> m x)
    0xD3A9C4 -> apply recOf rec (f :: EDG -> m x)
    0xD3A9C5 -> apply recOf rec (f :: EFG -> m x)
    0xD3A9C6 -> apply recOf rec (f :: ERG -> m x)
    0xD3A9C7 -> apply recOf rec (f :: EOG -> m x)
    0xD3A9C9 -> apply recOf rec (f :: EAG -> m x)
    0xD3A9CA -> apply recOf rec (f :: EDM -> m x)
    0xD3A9CB -> apply recOf rec (f :: EPM -> m x)
    0xD3A9CC -> apply recOf rec (f :: EMM -> m x)
    0xD3A9CD -> apply recOf rec (f :: EFM -> m x)
    0xD3A9CE -> apply recOf rec (f :: ER -> m x)
    0xD3A9D9 -> apply recOf rec (f :: ESG -> m x)
    0xD3A9DF -> apply recOf rec (f :: EMO -> m x)
    0xD3A9E3 -> apply recOf rec (f :: EDX -> m x)
    0xD3A9EB -> apply recOf rec (f :: EBC -> m x)
    0xD3A9FB -> apply recOf rec (f :: EIM -> m x)
    0xD3AAE7 -> apply recOf rec (f :: LNC -> m x)
    0xD3AB77 -> apply recOf rec (f :: MCA -> m x)
    0xD3AB88 -> apply recOf rec (f :: MMT -> m x)
    0xD3AB89 -> apply recOf rec (f :: FNN -> m x)
    0xD3AB8A -> apply recOf rec (f :: MCF -> m x)
    0xD3AB92 -> apply recOf rec (f :: MCD -> m x)
    0xD3ABAF -> apply recOf rec (f :: MPG -> m x)
    0xD3ABBB -> apply recOf rec (f :: MGO -> m x)
    0xD3ABC3 -> apply recOf rec (f :: MDR -> m x)
    0xD3ABCC -> apply recOf rec (f :: IMM -> m x)
    0xD3ABD8 -> apply recOf rec (f :: MPO -> m x)
    0xD3ABEA -> apply recOf rec (f :: MSU -> m x)
    0xD3ABEB -> apply recOf rec (f :: MBC -> m x)
    0xD3ABFB -> apply recOf rec (f :: MIO -> m x)
    0xD3AC6B -> apply recOf rec (f :: OBP -> m x)
    0xD3AC7B -> apply recOf rec (f :: ICP -> m x)
    0xD3AC89 -> apply recOf rec (f :: FNP -> m x)
    0xD3ACAF -> apply recOf rec (f :: PGP1 -> m x)
    0xD3AE89 -> apply recOf rec (f :: FNO -> m x)
    0xD3AF5F -> apply recOf rec (f :: IPS -> m x)
    0xD3AFAF -> apply recOf rec (f :: IPG -> m x)
    0xD3AFC3 -> apply recOf rec (f :: IOB -> m x)
    0xD3AFD8 -> apply recOf rec (f :: IPO -> m x)
    0xD3B077 -> apply recOf rec (f :: CAT -> m x)
    0xD3B15F -> apply recOf rec (f :: MPS -> m x)
    0xD3B18A -> apply recOf rec (f :: MCF1 -> m x)
    0xD3B19B -> apply recOf rec (f :: PTD -> m x)
    0xD3B1AF -> apply recOf rec (f :: PGP -> m x)
    0xD3B1DF -> apply recOf rec (f :: MMO -> m x)
    0xD3B288 -> apply recOf rec (f :: PFC -> m x)
    0xD3B2A7 -> apply recOf rec (f :: IEL -> m x)
    0xD3B490 -> apply recOf rec (f :: LLE -> m x)
    0xD3EE7B -> apply recOf rec (f :: IRD -> m x)
    0xD3EE89 -> apply recOf rec (f :: FNG -> m x)
    0xD3EE92 -> apply recOf rec (f :: OCD -> m x)
    0xD3EE9B -> apply recOf rec (f :: PTX -> m x)
    0xD3EEBB -> apply recOf rec (f :: GAD -> m x)
    0xD3EEEB -> apply recOf rec (f :: BDA -> m x)
    0xD3EEEE -> apply recOf rec (f :: NOP -> m x)
    0xD3EEFB -> apply recOf rec (f :: IPD -> m x)
    _        -> apply recOf rec (f :: Unknown -> m x)

instance ChunkBuf MCF_ N0 Buffer2 where
    chunkCon = MCF_
    chunkDecon (MCF_ x) = x
    chunkTypeLookup = lookupMCF
    chunkApply = applyMCF

applyMCF :: forall m x. (MonadIO m)
    => N0 -> MCF_ -> (forall a. (Rec a) => (a -> m x)) -> m x
applyMCF x rec f = case x of
    _        -> apply recOf rec (f :: MCF_T -> m x)

instance RecChunk MCF MCF_ N0 Buffer2 where
    readChunks r = mcf_Chunks r
    writeChunks r io = do
        cs <- io
        return $ r { mcf_Chunks = cs }

instance Rec MCF_ where
    recSizeOf c = 2 + (snd $ chunkToPStrLen c)

instance Rec MCF where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MCF a01 a02 a03
    recPut bh r = do put bh $ mcf_Type r; put bh $ mcf_ r; put bh $ mcf_Chunks r; return ()
    recSizeOf r = sum [ sizeOf $ mcf_Type r, sizeOf $ mcf_ r, sizeOf $ mcf_Chunks r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mcf_Type r), viewField "_" (viewNumber $ mcf_ r), viewField "Chunks" (viewChunks $ mcf_Chunks r) ]
    recType = fromEnum . mcf_Type

instance Rec MCF1_Data where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; a04 <- get bh; a05 <- get bh; a06 <- get bh; a07 <- get bh; a08 <- get bh; return $ MCF1_Data a01 a02 a03 a04 a05 a06 a07 a08
    recPut bh r = do put bh $ mcf1_CodedFontLocalId r; put bh $ mcf1_Reserved1 r; put bh $ mcf1_CodedFontResourceSectionId r; put bh $ mcf1_Reserved2 r; put bh $ mcf1_CodedFontName r; put bh $ mcf1_CodePageName r; put bh $ mcf1_FontCharacterSetName r; put bh $ mcf1_CharacterRotation r; return ()
    recSizeOf r = sum [ sizeOf $ mcf1_CodedFontLocalId r, sizeOf $ mcf1_Reserved1 r, sizeOf $ mcf1_CodedFontResourceSectionId r, sizeOf $ mcf1_Reserved2 r, sizeOf $ mcf1_CodedFontName r, sizeOf $ mcf1_CodePageName r, sizeOf $ mcf1_FontCharacterSetName r, sizeOf $ mcf1_CharacterRotation r ]
    recView r = viewRecord (typeOf r) [ viewField "CodedFontLocalId" (viewNumber $ mcf1_CodedFontLocalId r), viewField "Reserved1" (viewString $ mcf1_Reserved1 r), viewField "CodedFontResourceSectionId" (viewNumber $ mcf1_CodedFontResourceSectionId r), viewField "Reserved2" (viewString $ mcf1_Reserved2 r), viewField "CodedFontName" (viewString $ mcf1_CodedFontName r), viewField "CodePageName" (viewString $ mcf1_CodePageName r), viewField "FontCharacterSetName" (viewString $ mcf1_FontCharacterSetName r), viewField "CharacterRotation" (viewNumber $ mcf1_CharacterRotation r) ]
    recType r = 0

instance RecData MCF1 MCF1_Data where
    readData r = mcf1_Data r
    writeData r cs = r { mcf1_Data = cs }

instance Rec MCF1 where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; a04 <- get bh; a05 <- get bh; return $ MCF1 a01 a02 a03 a04 a05
    recPut bh r = do put bh $ mcf1_Type r; put bh $ mcf1_ r; put bh $ mcf1_RepeatingGroupLength r; put bh $ mcf1__ r; put bh $ mcf1_Data r; return ()
    recSizeOf r = sum [ sizeOf $ mcf1_Type r, sizeOf $ mcf1_ r, sizeOf $ mcf1_RepeatingGroupLength r, sizeOf $ mcf1__ r, sizeOf $ mcf1_Data r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mcf1_Type r), viewField "_" (viewNumber $ mcf1_ r), viewField "RepeatingGroupLength" (viewNumber $ mcf1_RepeatingGroupLength r), viewField "__" (viewString $ mcf1__ r), viewField "Data" (viewData $ mcf1_Data r) ]
    recType = fromEnum . mcf1_Type

instance Rec MDD where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MDD a01 a02 a03
    recPut bh r = do put bh $ mdd_Type r; put bh $ mdd_ r; put bh $ mdd r; return ()
    recSizeOf r = sum [ sizeOf $ mdd_Type r, sizeOf $ mdd_ r, sizeOf $ mdd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mdd_Type r), viewField "_" (viewNumber $ mdd_ r), viewField "" (viewNStr $ mdd r) ]
    recType = fromEnum . mdd_Type

instance Rec MDR where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MDR a01 a02 a03
    recPut bh r = do put bh $ mdr_Type r; put bh $ mdr_ r; put bh $ mdr r; return ()
    recSizeOf r = sum [ sizeOf $ mdr_Type r, sizeOf $ mdr_ r, sizeOf $ mdr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mdr_Type r), viewField "_" (viewNumber $ mdr_ r), viewField "" (viewNStr $ mdr r) ]
    recType = fromEnum . mdr_Type

instance Rec MFC where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MFC a01 a02 a03
    recPut bh r = do put bh $ mfc_Type r; put bh $ mfc_ r; put bh $ mfc r; return ()
    recSizeOf r = sum [ sizeOf $ mfc_Type r, sizeOf $ mfc_ r, sizeOf $ mfc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mfc_Type r), viewField "_" (viewNumber $ mfc_ r), viewField "" (viewNStr $ mfc r) ]
    recType = fromEnum . mfc_Type

instance Rec MGO where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MGO a01 a02 a03
    recPut bh r = do put bh $ mgo_Type r; put bh $ mgo_ r; put bh $ mgo r; return ()
    recSizeOf r = sum [ sizeOf $ mgo_Type r, sizeOf $ mgo_ r, sizeOf $ mgo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mgo_Type r), viewField "_" (viewNumber $ mgo_ r), viewField "" (viewNStr $ mgo r) ]
    recType = fromEnum . mgo_Type

instance Rec MIO where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MIO a01 a02 a03
    recPut bh r = do put bh $ mio_Type r; put bh $ mio_ r; put bh $ mio r; return ()
    recSizeOf r = sum [ sizeOf $ mio_Type r, sizeOf $ mio_ r, sizeOf $ mio r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mio_Type r), viewField "_" (viewNumber $ mio_ r), viewField "" (viewNStr $ mio r) ]
    recType = fromEnum . mio_Type

instance Rec MMC where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MMC a01 a02 a03
    recPut bh r = do put bh $ mmc_Type r; put bh $ mmc_ r; put bh $ mmc r; return ()
    recSizeOf r = sum [ sizeOf $ mmc_Type r, sizeOf $ mmc_ r, sizeOf $ mmc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mmc_Type r), viewField "_" (viewNumber $ mmc_ r), viewField "" (viewNStr $ mmc r) ]
    recType = fromEnum . mmc_Type

instance Rec MMO where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MMO a01 a02 a03
    recPut bh r = do put bh $ mmo_Type r; put bh $ mmo_ r; put bh $ mmo r; return ()
    recSizeOf r = sum [ sizeOf $ mmo_Type r, sizeOf $ mmo_ r, sizeOf $ mmo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mmo_Type r), viewField "_" (viewNumber $ mmo_ r), viewField "" (viewNStr $ mmo r) ]
    recType = fromEnum . mmo_Type

instance Rec MMT where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MMT a01 a02 a03
    recPut bh r = do put bh $ mmt_Type r; put bh $ mmt_ r; put bh $ mmt r; return ()
    recSizeOf r = sum [ sizeOf $ mmt_Type r, sizeOf $ mmt_ r, sizeOf $ mmt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mmt_Type r), viewField "_" (viewNumber $ mmt_ r), viewField "" (viewNStr $ mmt r) ]
    recType = fromEnum . mmt_Type

instance Rec MPG where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MPG a01 a02 a03
    recPut bh r = do put bh $ mpg_Type r; put bh $ mpg_ r; put bh $ mpg r; return ()
    recSizeOf r = sum [ sizeOf $ mpg_Type r, sizeOf $ mpg_ r, sizeOf $ mpg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mpg_Type r), viewField "_" (viewNumber $ mpg_ r), viewField "" (viewNStr $ mpg r) ]
    recType = fromEnum . mpg_Type

instance Rec MPO where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MPO a01 a02 a03
    recPut bh r = do put bh $ mpo_Type r; put bh $ mpo_ r; put bh $ mpo r; return ()
    recSizeOf r = sum [ sizeOf $ mpo_Type r, sizeOf $ mpo_ r, sizeOf $ mpo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mpo_Type r), viewField "_" (viewNumber $ mpo_ r), viewField "" (viewNStr $ mpo r) ]
    recType = fromEnum . mpo_Type

instance Rec MPS where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MPS a01 a02 a03
    recPut bh r = do put bh $ mps_Type r; put bh $ mps_ r; put bh $ mps r; return ()
    recSizeOf r = sum [ sizeOf $ mps_Type r, sizeOf $ mps_ r, sizeOf $ mps r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mps_Type r), viewField "_" (viewNumber $ mps_ r), viewField "" (viewNStr $ mps r) ]
    recType = fromEnum . mps_Type

instance Rec MSU where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ MSU a01 a02 a03
    recPut bh r = do put bh $ msu_Type r; put bh $ msu_ r; put bh $ msu r; return ()
    recSizeOf r = sum [ sizeOf $ msu_Type r, sizeOf $ msu_ r, sizeOf $ msu r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ msu_Type r), viewField "_" (viewNumber $ msu_ r), viewField "" (viewNStr $ msu r) ]
    recType = fromEnum . msu_Type

instance Rec NOP where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ NOP a01 a02 a03
    recPut bh r = do put bh $ nop_Type r; put bh $ nop_ r; put bh $ nop r; return ()
    recSizeOf r = sum [ sizeOf $ nop_Type r, sizeOf $ nop_ r, sizeOf $ nop r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ nop_Type r), viewField "_" (viewNumber $ nop_ r), viewField "" (viewNStr $ nop r) ]
    recType = fromEnum . nop_Type

instance Rec OBD where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ OBD a01 a02 a03
    recPut bh r = do put bh $ obd_Type r; put bh $ obd_ r; put bh $ obd r; return ()
    recSizeOf r = sum [ sizeOf $ obd_Type r, sizeOf $ obd_ r, sizeOf $ obd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ obd_Type r), viewField "_" (viewNumber $ obd_ r), viewField "" (viewNStr $ obd r) ]
    recType = fromEnum . obd_Type

instance Rec OBP where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ OBP a01 a02 a03
    recPut bh r = do put bh $ obp_Type r; put bh $ obp_ r; put bh $ obp r; return ()
    recSizeOf r = sum [ sizeOf $ obp_Type r, sizeOf $ obp_ r, sizeOf $ obp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ obp_Type r), viewField "_" (viewNumber $ obp_ r), viewField "" (viewNStr $ obp r) ]
    recType = fromEnum . obp_Type

instance Rec OCD where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ OCD a01 a02 a03
    recPut bh r = do put bh $ ocd_Type r; put bh $ ocd_ r; put bh $ ocd r; return ()
    recSizeOf r = sum [ sizeOf $ ocd_Type r, sizeOf $ ocd_ r, sizeOf $ ocd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ocd_Type r), viewField "_" (viewNumber $ ocd_ r), viewField "" (viewNStr $ ocd r) ]
    recType = fromEnum . ocd_Type

instance Rec PFC where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ PFC a01 a02 a03
    recPut bh r = do put bh $ pfc_Type r; put bh $ pfc_ r; put bh $ pfc r; return ()
    recSizeOf r = sum [ sizeOf $ pfc_Type r, sizeOf $ pfc_ r, sizeOf $ pfc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pfc_Type r), viewField "_" (viewNumber $ pfc_ r), viewField "" (viewNStr $ pfc r) ]
    recType = fromEnum . pfc_Type

instance Rec PGD where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; a04 <- get bh; a05 <- get bh; a06 <- get bh; a07 <- get bh; a08 <- get bh; a09 <- get bh; a10 <- get bh; a11 <- get bh; return $ PGD a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11
    recPut bh r = do put bh $ pgd_Type r; put bh $ pgd_ r; put bh $ pgd_XUnitBase r; put bh $ pgd_YUnitBase r; put bh $ pgd_XLUnitsperUnitBase r; put bh $ pgd_YLUnitsperUnitBase r; put bh $ pgd_Reserved1 r; put bh $ pgd_XPageSize r; put bh $ pgd_Reserved2 r; put bh $ pgd_YPageSize r; put bh $ pgd__ r; return ()
    recSizeOf r = sum [ sizeOf $ pgd_Type r, sizeOf $ pgd_ r, sizeOf $ pgd_XUnitBase r, sizeOf $ pgd_YUnitBase r, sizeOf $ pgd_XLUnitsperUnitBase r, sizeOf $ pgd_YLUnitsperUnitBase r, sizeOf $ pgd_Reserved1 r, sizeOf $ pgd_XPageSize r, sizeOf $ pgd_Reserved2 r, sizeOf $ pgd_YPageSize r, sizeOf $ pgd__ r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pgd_Type r), viewField "_" (viewNumber $ pgd_ r), viewField "XUnitBase" (viewNumber $ pgd_XUnitBase r), viewField "YUnitBase" (viewNumber $ pgd_YUnitBase r), viewField "XLUnitsperUnitBase" (viewNumber $ pgd_XLUnitsperUnitBase r), viewField "YLUnitsperUnitBase" (viewNumber $ pgd_YLUnitsperUnitBase r), viewField "Reserved1" (viewNumber $ pgd_Reserved1 r), viewField "XPageSize" (viewNumber $ pgd_XPageSize r), viewField "Reserved2" (viewNumber $ pgd_Reserved2 r), viewField "YPageSize" (viewNumber $ pgd_YPageSize r), viewField "__" (viewNStr $ pgd__ r) ]
    recType = fromEnum . pgd_Type

instance Rec PGP where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ PGP a01 a02 a03
    recPut bh r = do put bh $ pgp_Type r; put bh $ pgp_ r; put bh $ pgp r; return ()
    recSizeOf r = sum [ sizeOf $ pgp_Type r, sizeOf $ pgp_ r, sizeOf $ pgp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pgp_Type r), viewField "_" (viewNumber $ pgp_ r), viewField "" (viewNStr $ pgp r) ]
    recType = fromEnum . pgp_Type

instance Rec PGP1 where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ PGP1 a01 a02 a03
    recPut bh r = do put bh $ pgp1_Type r; put bh $ pgp1_ r; put bh $ pgp1 r; return ()
    recSizeOf r = sum [ sizeOf $ pgp1_Type r, sizeOf $ pgp1_ r, sizeOf $ pgp1 r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pgp1_Type r), viewField "_" (viewNumber $ pgp1_ r), viewField "" (viewNStr $ pgp1 r) ]
    recType = fromEnum . pgp1_Type

instance Rec PMC where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ PMC a01 a02 a03
    recPut bh r = do put bh $ pmc_Type r; put bh $ pmc_ r; put bh $ pmc r; return ()
    recSizeOf r = sum [ sizeOf $ pmc_Type r, sizeOf $ pmc_ r, sizeOf $ pmc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pmc_Type r), viewField "_" (viewNumber $ pmc_ r), viewField "" (viewNStr $ pmc r) ]
    recType = fromEnum . pmc_Type

instance Rec PTD where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ PTD a01 a02 a03
    recPut bh r = do put bh $ ptd_Type r; put bh $ ptd_ r; put bh $ ptd r; return ()
    recSizeOf r = sum [ sizeOf $ ptd_Type r, sizeOf $ ptd_ r, sizeOf $ ptd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptd_Type r), viewField "_" (viewNumber $ ptd_ r), viewField "" (viewNStr $ ptd r) ]
    recType = fromEnum . ptd_Type

instance Rec PTD1 where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; return $ PTD1 a01 a02 a03
    recPut bh r = do put bh $ ptd1_Type r; put bh $ ptd1_ r; put bh $ ptd1 r; return ()
    recSizeOf r = sum [ sizeOf $ ptd1_Type r, sizeOf $ ptd1_ r, sizeOf $ ptd1 r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptd1_Type r), viewField "_" (viewNumber $ ptd1_ r), viewField "" (viewNStr $ ptd1 r) ]
    recType = fromEnum . ptd1_Type

instance RecChunk TLE T_ N1 Buffer1 where
    readChunks r = tle_Chunks r
    writeChunks r io = do
        cs <- io
        return $ r { tle_Chunks = cs }

instance Rec TLE where
    recGet bh = do a01 <- get bh; a02 <- get bh; a03 <- get bh; a04 <- get bh; a05 <- get bh; a06 <- get bh; a07 <- get bh; a08 <- get bh; a09 <- get bh; a10 <- get bh; return $ TLE a01 a02 a03 a04 a05 a06 a07 a08 a09 a10
    recPut bh r = do put bh $ tle_Type r; put bh $ tle_ r; put bh $ tle_Chunks r; put bh $ tle_XUnitBase r; put bh $ tle_YUnitBase r; put bh $ tle_XLUnitsperUnitBase r; put bh $ tle_YLUnitsperUnitBase r; put bh $ tle_XPageSize r; put bh $ tle_YPageSize r; put bh $ tle__ r; return ()
    recSizeOf r = sum [ sizeOf $ tle_Type r, sizeOf $ tle_ r, sizeOf $ tle_Chunks r, sizeOf $ tle_XUnitBase r, sizeOf $ tle_YUnitBase r, sizeOf $ tle_XLUnitsperUnitBase r, sizeOf $ tle_YLUnitsperUnitBase r, sizeOf $ tle_XPageSize r, sizeOf $ tle_YPageSize r, sizeOf $ tle__ r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ tle_Type r), viewField "_" (viewNumber $ tle_ r), viewField "Chunks" (viewChunks $ tle_Chunks r), viewField "XUnitBase" (viewNumber $ tle_XUnitBase r), viewField "YUnitBase" (viewNumber $ tle_YUnitBase r), viewField "XLUnitsperUnitBase" (viewNumber $ tle_XLUnitsperUnitBase r), viewField "YLUnitsperUnitBase" (viewNumber $ tle_YLUnitsperUnitBase r), viewField "XPageSize" (viewNumber $ tle_XPageSize r), viewField "YPageSize" (viewNumber $ tle_YPageSize r), viewField "__" (viewNStr $ tle__ r) ]
    recType = fromEnum . tle_Type

instance RecChunk MCF_T T_ N1 Buffer1 where
    readChunks r = mcf_t_Chunks r
    writeChunks r io = do
        cs <- io
        return $ r { mcf_t_Chunks = cs }

instance Rec MCF_T where
    recGet bh = do a01 <- get bh; return $ MCF_T a01
    recPut bh r = do put bh $ mcf_t_Chunks r; return ()
    recSizeOf r = sum [ sizeOf $ mcf_t_Chunks r ]
    recView r = viewRecord (typeOf r) [ viewField "Chunks" (viewChunks $ mcf_t_Chunks r) ]
    recType r = 0

