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

applyAFP :: (MonadIO m) => N3 -> AFP_ -> (forall a. (Rec a) => (a -> m b)) -> m b
applyAFP x = case x of
    0xD38C87 -> apply (recOf :: AFP_ -> IO CPI)
    0xD38C89 -> apply (recOf :: AFP_ -> IO FNI)
    0xD38C8A -> apply (recOf :: AFP_ -> IO CFI)
    0xD3A088 -> apply (recOf :: AFP_ -> IO MFC)
    0xD3A090 -> apply (recOf :: AFP_ -> IO TLE)
    0xD3A288 -> apply (recOf :: AFP_ -> IO MCC)
    0xD3A289 -> apply (recOf :: AFP_ -> IO FNM)
    0xD3A66B -> apply (recOf :: AFP_ -> IO OBD)
    0xD3A67B -> apply (recOf :: AFP_ -> IO IID)
    0xD3A687 -> apply (recOf :: AFP_ -> IO CPD)
    0xD3A688 -> apply (recOf :: AFP_ -> IO MDD)
    0xD3A689 -> apply (recOf :: AFP_ -> IO FND)
    0xD3A692 -> apply (recOf :: AFP_ -> IO CDD)
    0xD3A69B -> apply (recOf :: AFP_ -> IO PTD1)
    0xD3A6AF -> apply (recOf :: AFP_ -> IO PGD)
    0xD3A6BB -> apply (recOf :: AFP_ -> IO GDD)
    0xD3A6C5 -> apply (recOf :: AFP_ -> IO FGD)
    0xD3A6E3 -> apply (recOf :: AFP_ -> IO DXD)
    0xD3A6E7 -> apply (recOf :: AFP_ -> IO LND)
    0xD3A6EB -> apply (recOf :: AFP_ -> IO BDD)
    0xD3A6FB -> apply (recOf :: AFP_ -> IO IDD)
    0xD3A77B -> apply (recOf :: AFP_ -> IO IOC)
    0xD3A787 -> apply (recOf :: AFP_ -> IO CPC)
    0xD3A788 -> apply (recOf :: AFP_ -> IO MMC)
    0xD3A789 -> apply (recOf :: AFP_ -> IO FNC)
    0xD3A78A -> apply (recOf :: AFP_ -> IO CFC)
    0xD3A79B -> apply (recOf :: AFP_ -> IO CTC)
    0xD3A7AF -> apply (recOf :: AFP_ -> IO PMC)
    0xD3A85F -> apply (recOf :: AFP_ -> IO BPS)
    0xD3A877 -> apply (recOf :: AFP_ -> IO BCA)
    0xD3A87B -> apply (recOf :: AFP_ -> IO BII)
    0xD3A887 -> apply (recOf :: AFP_ -> IO BCP)
    0xD3A889 -> apply (recOf :: AFP_ -> IO BFN)
    0xD3A88A -> apply (recOf :: AFP_ -> IO BCF)
    0xD3A892 -> apply (recOf :: AFP_ -> IO BOC)
    0xD3A89B -> apply (recOf :: AFP_ -> IO BPT)
    0xD3A8A7 -> apply (recOf :: AFP_ -> IO BDI)
    0xD3A8A8 -> apply (recOf :: AFP_ -> IO BDT)
    0xD3A8AD -> apply (recOf :: AFP_ -> IO BNG)
    0xD3A8AF -> apply (recOf :: AFP_ -> IO BPG)
    0xD3A8BB -> apply (recOf :: AFP_ -> IO BGR)
    0xD3A8C4 -> apply (recOf :: AFP_ -> IO BDG)
    0xD3A8C5 -> apply (recOf :: AFP_ -> IO BFG)
    0xD3A8C6 -> apply (recOf :: AFP_ -> IO BRG)
    0xD3A8C7 -> apply (recOf :: AFP_ -> IO BOG)
    0xD3A8C9 -> apply (recOf :: AFP_ -> IO BAG)
    0xD3A8CA -> apply (recOf :: AFP_ -> IO BDM)
    0xD3A8CB -> apply (recOf :: AFP_ -> IO BPM)
    0xD3A8CC -> apply (recOf :: AFP_ -> IO BMM)
    0xD3A8CD -> apply (recOf :: AFP_ -> IO BFM)
    0xD3A8CE -> apply (recOf :: AFP_ -> IO BR)
    0xD3A8D9 -> apply (recOf :: AFP_ -> IO BSG)
    0xD3A8DF -> apply (recOf :: AFP_ -> IO BMO)
    0xD3A8E3 -> apply (recOf :: AFP_ -> IO BDX)
    0xD3A8EB -> apply (recOf :: AFP_ -> IO BBC)
    0xD3A8FB -> apply (recOf :: AFP_ -> IO BIM)
    0xD3A95F -> apply (recOf :: AFP_ -> IO EPS)
    0xD3A977 -> apply (recOf :: AFP_ -> IO ECA)
    0xD3A97B -> apply (recOf :: AFP_ -> IO EII)
    0xD3A987 -> apply (recOf :: AFP_ -> IO ECP)
    0xD3A989 -> apply (recOf :: AFP_ -> IO EFN)
    0xD3A98A -> apply (recOf :: AFP_ -> IO ECF)
    0xD3A992 -> apply (recOf :: AFP_ -> IO EOC)
    0xD3A99B -> apply (recOf :: AFP_ -> IO EPT)
    0xD3A9A7 -> apply (recOf :: AFP_ -> IO EDI)
    0xD3A9A8 -> apply (recOf :: AFP_ -> IO EDT)
    0xD3A9AD -> apply (recOf :: AFP_ -> IO ENG)
    0xD3A9AF -> apply (recOf :: AFP_ -> IO EPG)
    0xD3A9BB -> apply (recOf :: AFP_ -> IO EGR)
    0xD3A9C4 -> apply (recOf :: AFP_ -> IO EDG)
    0xD3A9C5 -> apply (recOf :: AFP_ -> IO EFG)
    0xD3A9C6 -> apply (recOf :: AFP_ -> IO ERG)
    0xD3A9C7 -> apply (recOf :: AFP_ -> IO EOG)
    0xD3A9C9 -> apply (recOf :: AFP_ -> IO EAG)
    0xD3A9CA -> apply (recOf :: AFP_ -> IO EDM)
    0xD3A9CB -> apply (recOf :: AFP_ -> IO EPM)
    0xD3A9CC -> apply (recOf :: AFP_ -> IO EMM)
    0xD3A9CD -> apply (recOf :: AFP_ -> IO EFM)
    0xD3A9CE -> apply (recOf :: AFP_ -> IO ER)
    0xD3A9D9 -> apply (recOf :: AFP_ -> IO ESG)
    0xD3A9DF -> apply (recOf :: AFP_ -> IO EMO)
    0xD3A9E3 -> apply (recOf :: AFP_ -> IO EDX)
    0xD3A9EB -> apply (recOf :: AFP_ -> IO EBC)
    0xD3A9FB -> apply (recOf :: AFP_ -> IO EIM)
    0xD3AAE7 -> apply (recOf :: AFP_ -> IO LNC)
    0xD3AB77 -> apply (recOf :: AFP_ -> IO MCA)
    0xD3AB88 -> apply (recOf :: AFP_ -> IO MMT)
    0xD3AB89 -> apply (recOf :: AFP_ -> IO FNN)
    0xD3AB8A -> apply (recOf :: AFP_ -> IO MCF)
    0xD3AB92 -> apply (recOf :: AFP_ -> IO MCD)
    0xD3ABAF -> apply (recOf :: AFP_ -> IO MPG)
    0xD3ABBB -> apply (recOf :: AFP_ -> IO MGO)
    0xD3ABC3 -> apply (recOf :: AFP_ -> IO MDR)
    0xD3ABCC -> apply (recOf :: AFP_ -> IO IMM)
    0xD3ABD8 -> apply (recOf :: AFP_ -> IO MPO)
    0xD3ABEA -> apply (recOf :: AFP_ -> IO MSU)
    0xD3ABEB -> apply (recOf :: AFP_ -> IO MBC)
    0xD3ABFB -> apply (recOf :: AFP_ -> IO MIO)
    0xD3AC6B -> apply (recOf :: AFP_ -> IO OBP)
    0xD3AC7B -> apply (recOf :: AFP_ -> IO ICP)
    0xD3AC89 -> apply (recOf :: AFP_ -> IO FNP)
    0xD3ACAF -> apply (recOf :: AFP_ -> IO PGP1)
    0xD3AE89 -> apply (recOf :: AFP_ -> IO FNO)
    0xD3AF5F -> apply (recOf :: AFP_ -> IO IPS)
    0xD3AFAF -> apply (recOf :: AFP_ -> IO IPG)
    0xD3AFC3 -> apply (recOf :: AFP_ -> IO IOB)
    0xD3AFD8 -> apply (recOf :: AFP_ -> IO IPO)
    0xD3B077 -> apply (recOf :: AFP_ -> IO CAT)
    0xD3B15F -> apply (recOf :: AFP_ -> IO MPS)
    0xD3B18A -> apply (recOf :: AFP_ -> IO MCF1)
    0xD3B19B -> apply (recOf :: AFP_ -> IO PTD)
    0xD3B1AF -> apply (recOf :: AFP_ -> IO PGP)
    0xD3B1DF -> apply (recOf :: AFP_ -> IO MMO)
    0xD3B288 -> apply (recOf :: AFP_ -> IO PFC)
    0xD3B2A7 -> apply (recOf :: AFP_ -> IO IEL)
    0xD3B490 -> apply (recOf :: AFP_ -> IO LLE)
    0xD3EE7B -> apply (recOf :: AFP_ -> IO IRD)
    0xD3EE89 -> apply (recOf :: AFP_ -> IO FNG)
    0xD3EE92 -> apply (recOf :: AFP_ -> IO OCD)
    0xD3EE9B -> apply (recOf :: AFP_ -> IO PTX)
    0xD3EEBB -> apply (recOf :: AFP_ -> IO GAD)
    0xD3EEEB -> apply (recOf :: AFP_ -> IO BDA)
    0xD3EEEE -> apply (recOf :: AFP_ -> IO NOP)
    0xD3EEFB -> apply (recOf :: AFP_ -> IO IPD)
    _        -> apply (recOf :: AFP_ -> IO Unknown)

instance ChunkBuf MCF_ N0 Buffer2 where
    chunkCon = MCF_
    chunkDecon (MCF_ x) = x
    chunkTypeLookup = lookupMCF
    chunkApply = applyMCF

applyMCF :: (MonadIO m) => N0 -> MCF_ -> (forall a. (Rec a) => (a -> m b)) -> m b
applyMCF x = case x of
    _    -> apply (recOf :: MCF_ -> IO MCF_T)

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

