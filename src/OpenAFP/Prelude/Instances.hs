
module OpenAFP.Prelude.Instances where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals
import OpenAFP.Prelude.InstanceT
import OpenAFP.Prelude.InstancePTX
import OpenAFP.Prelude.InstanceAFP
import qualified Data.ByteString as S

instance Rec T_ where
    recSizeOf c = 1 + (S.length $ packChunk c)

instance Chunk AFP_ where
    type N AFP_ = N3
    type BufOf AFP_ = Buffer2
    mkChunk = AFP_
    chunkDecon (AFP_ x y) = (x, y)
    chunkTypeLookup = lookupAFP
    chunkApply = applyAFP

apply :: (Chunk c, Rec r) => c -> (r -> t) -> t
apply c f = f (decodeChunk c)

applyAFP :: forall x. N3 -> AFP_ -> (forall a. (Rec a) => (a -> x)) -> x
applyAFP x rec f = case x of
    0xD38C87 -> apply rec (f :: CPI -> x)
    0xD38C89 -> apply rec (f :: FNI -> x)
    0xD38C8A -> apply rec (f :: CFI -> x)
    0xD3A088 -> apply rec (f :: MFC -> x)
    0xD3A090 -> apply rec (f :: TLE -> x)
    0xD3A288 -> apply rec (f :: MCC -> x)
    0xD3A289 -> apply rec (f :: FNM -> x)
    0xD3A66B -> apply rec (f :: OBD -> x)
    0xD3A67B -> apply rec (f :: IID -> x)
    0xD3A687 -> apply rec (f :: CPD -> x)
    0xD3A688 -> apply rec (f :: MDD -> x)
    0xD3A689 -> apply rec (f :: FND -> x)
    0xD3A692 -> apply rec (f :: CDD -> x)
    0xD3A69B -> apply rec (f :: PTD1 -> x)
    0xD3A6AF -> apply rec (f :: PGD -> x)
    0xD3A6BB -> apply rec (f :: GDD -> x)
    0xD3A6C5 -> apply rec (f :: FGD -> x)
    0xD3A6E3 -> apply rec (f :: DXD -> x)
    0xD3A6E7 -> apply rec (f :: LND -> x)
    0xD3A6EB -> apply rec (f :: BDD -> x)
    0xD3A6FB -> apply rec (f :: IDD -> x)
    0xD3A77B -> apply rec (f :: IOC -> x)
    0xD3A787 -> apply rec (f :: CPC -> x)
    0xD3A788 -> apply rec (f :: MMC -> x)
    0xD3A789 -> apply rec (f :: FNC -> x)
    0xD3A78A -> apply rec (f :: CFC -> x)
    0xD3A79B -> apply rec (f :: CTC -> x)
    0xD3A7AF -> apply rec (f :: PMC -> x)
    0xD3A85F -> apply rec (f :: BPS -> x)
    0xD3A877 -> apply rec (f :: BCA -> x)
    0xD3A87B -> apply rec (f :: BII -> x)
    0xD3A887 -> apply rec (f :: BCP -> x)
    0xD3A889 -> apply rec (f :: BFN -> x)
    0xD3A88A -> apply rec (f :: BCF -> x)
    0xD3A892 -> apply rec (f :: BOC -> x)
    0xD3A89B -> apply rec (f :: BPT -> x)
    0xD3A8A7 -> apply rec (f :: BDI -> x)
    0xD3A8A8 -> apply rec (f :: BDT -> x)
    0xD3A8AD -> apply rec (f :: BNG -> x)
    0xD3A8AF -> apply rec (f :: BPG -> x)
    0xD3A8BB -> apply rec (f :: BGR -> x)
    0xD3A8C4 -> apply rec (f :: BDG -> x)
    0xD3A8C5 -> apply rec (f :: BFG -> x)
    0xD3A8C6 -> apply rec (f :: BRG -> x)
    0xD3A8C7 -> apply rec (f :: BOG -> x)
    0xD3A8C9 -> apply rec (f :: BAG -> x)
    0xD3A8CA -> apply rec (f :: BDM -> x)
    0xD3A8CB -> apply rec (f :: BPM -> x)
    0xD3A8CC -> apply rec (f :: BMM -> x)
    0xD3A8CD -> apply rec (f :: BFM -> x)
    0xD3A8CE -> apply rec (f :: BR -> x)
    0xD3A8D9 -> apply rec (f :: BSG -> x)
    0xD3A8DF -> apply rec (f :: BMO -> x)
    0xD3A8E3 -> apply rec (f :: BDX -> x)
    0xD3A8EB -> apply rec (f :: BBC -> x)
    0xD3A8FB -> apply rec (f :: BIM -> x)
    0xD3A95F -> apply rec (f :: EPS -> x)
    0xD3A977 -> apply rec (f :: ECA -> x)
    0xD3A97B -> apply rec (f :: EII -> x)
    0xD3A987 -> apply rec (f :: ECP -> x)
    0xD3A989 -> apply rec (f :: EFN -> x)
    0xD3A98A -> apply rec (f :: ECF -> x)
    0xD3A992 -> apply rec (f :: EOC -> x)
    0xD3A99B -> apply rec (f :: EPT -> x)
    0xD3A9A7 -> apply rec (f :: EDI -> x)
    0xD3A9A8 -> apply rec (f :: EDT -> x)
    0xD3A9AD -> apply rec (f :: ENG -> x)
    0xD3A9AF -> apply rec (f :: EPG -> x)
    0xD3A9BB -> apply rec (f :: EGR -> x)
    0xD3A9C4 -> apply rec (f :: EDG -> x)
    0xD3A9C5 -> apply rec (f :: EFG -> x)
    0xD3A9C6 -> apply rec (f :: ERG -> x)
    0xD3A9C7 -> apply rec (f :: EOG -> x)
    0xD3A9C9 -> apply rec (f :: EAG -> x)
    0xD3A9CA -> apply rec (f :: EDM -> x)
    0xD3A9CB -> apply rec (f :: EPM -> x)
    0xD3A9CC -> apply rec (f :: EMM -> x)
    0xD3A9CD -> apply rec (f :: EFM -> x)
    0xD3A9CE -> apply rec (f :: ER -> x)
    0xD3A9D9 -> apply rec (f :: ESG -> x)
    0xD3A9DF -> apply rec (f :: EMO -> x)
    0xD3A9E3 -> apply rec (f :: EDX -> x)
    0xD3A9EB -> apply rec (f :: EBC -> x)
    0xD3A9FB -> apply rec (f :: EIM -> x)
    0xD3AAE7 -> apply rec (f :: LNC -> x)
    0xD3AB77 -> apply rec (f :: MCA -> x)
    0xD3AB88 -> apply rec (f :: MMT -> x)
    0xD3AB89 -> apply rec (f :: FNN -> x)
    0xD3AB8A -> apply rec (f :: MCF -> x)
    0xD3AB92 -> apply rec (f :: MCD -> x)
    0xD3ABAF -> apply rec (f :: MPG -> x)
    0xD3ABBB -> apply rec (f :: MGO -> x)
    0xD3ABC3 -> apply rec (f :: MDR -> x)
    0xD3ABCC -> apply rec (f :: IMM -> x)
    0xD3ABD8 -> apply rec (f :: MPO -> x)
    0xD3ABEA -> apply rec (f :: MSU -> x)
    0xD3ABEB -> apply rec (f :: MBC -> x)
    0xD3ABFB -> apply rec (f :: MIO -> x)
    0xD3AC6B -> apply rec (f :: OBP -> x)
    0xD3AC7B -> apply rec (f :: ICP -> x)
    0xD3AC89 -> apply rec (f :: FNP -> x)
    0xD3ACAF -> apply rec (f :: PGP1 -> x)
    0xD3AE89 -> apply rec (f :: FNO -> x)
    0xD3AF5F -> apply rec (f :: IPS -> x)
    0xD3AFAF -> apply rec (f :: IPG -> x)
    0xD3AFC3 -> apply rec (f :: IOB -> x)
    0xD3AFD8 -> apply rec (f :: IPO -> x)
    0xD3B077 -> apply rec (f :: CAT -> x)
    0xD3B15F -> apply rec (f :: MPS -> x)
    0xD3B18A -> apply rec (f :: MCF1 -> x)
    0xD3B19B -> apply rec (f :: PTD -> x)
    0xD3B1AF -> apply rec (f :: PGP -> x)
    0xD3B1DF -> apply rec (f :: MMO -> x)
    0xD3B288 -> apply rec (f :: PFC -> x)
    0xD3B2A7 -> apply rec (f :: IEL -> x)
    0xD3B490 -> apply rec (f :: LLE -> x)
    0xD3EE7B -> apply rec (f :: IRD -> x)
    0xD3EE89 -> apply rec (f :: FNG -> x)
    0xD3EE92 -> apply rec (f :: OCD -> x)
    0xD3EE9B -> apply rec (f :: PTX -> x)
    0xD3EEBB -> apply rec (f :: GAD -> x)
    0xD3EEEB -> apply rec (f :: BDA -> x)
    0xD3EEEE -> apply rec (f :: NOP -> x)
    0xD3EEFB -> apply rec (f :: IPD -> x)
    _        -> apply rec (f :: Unknown -> x)

instance Chunk MCF_ where
    type N MCF_ = N0
    type BufOf MCF_ = Buffer2
    mkChunk = MCF_
    chunkDecon (MCF_ x y) = (x, y)
    chunkTypeLookup = lookupMCF
    chunkApply = applyMCF

applyMCF :: forall x. N0 -> MCF_ -> (forall a. (Rec a) => (a -> x)) -> x
applyMCF x rec f = case x of
    _        -> apply rec (f :: MCF_T -> x)

instance RecChunk MCF where
    type ChunkOf MCF = MCF_
    readChunks r = mcf_Chunks r
    writeChunks r io = do
        cs <- io
        return $ r { mcf_Chunks = cs }

instance Rec MCF_ where
    recSizeOf c = 2 + (S.length $ packChunk c)

instance Rec MCF where
    recGet = do a01 <- get; a02 <- get; a03 <- getList; return $ MCF a01 a02 a03
    recPut r = do put $ mcf_Type r; put $ mcf_ r; putList $ mcf_Chunks r; return ()
    recSizeOf r = sum [ sizeOf $ mcf_Type r, sizeOf $ mcf_ r, sizeOf $ mcf_Chunks r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mcf_Type r), viewField "_" (viewNumber $ mcf_ r), viewField "Chunks" (viewChunks $ mcf_Chunks r) ]
    recType = fromEnum . mcf_Type

instance Rec MCF1_Data where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; return $ MCF1_Data a01 a02 a03 a04 a05 a06 a07 a08
    recPut r = do put $ mcf1_CodedFontLocalId r; put $ mcf1_Reserved1 r; put $ mcf1_CodedFontResourceSectionId r; put $ mcf1_Reserved2 r; put $ mcf1_CodedFontName r; put $ mcf1_CodePageName r; put $ mcf1_FontCharacterSetName r; put $ mcf1_CharacterRotation r; return ()
    recSizeOf r = sum [ sizeOf $ mcf1_CodedFontLocalId r, sizeOf $ mcf1_Reserved1 r, sizeOf $ mcf1_CodedFontResourceSectionId r, sizeOf $ mcf1_Reserved2 r, sizeOf $ mcf1_CodedFontName r, sizeOf $ mcf1_CodePageName r, sizeOf $ mcf1_FontCharacterSetName r, sizeOf $ mcf1_CharacterRotation r ]
    recView r = viewRecord (typeOf r) [ viewField "CodedFontLocalId" (viewNumber $ mcf1_CodedFontLocalId r), viewField "Reserved1" (viewString $ mcf1_Reserved1 r), viewField "CodedFontResourceSectionId" (viewNumber $ mcf1_CodedFontResourceSectionId r), viewField "Reserved2" (viewString $ mcf1_Reserved2 r), viewField "CodedFontName" (viewString $ mcf1_CodedFontName r), viewField "CodePageName" (viewString $ mcf1_CodePageName r), viewField "FontCharacterSetName" (viewString $ mcf1_FontCharacterSetName r), viewField "CharacterRotation" (viewNumber $ mcf1_CharacterRotation r) ]
    recType r = 0

instance RecData MCF1 MCF1_Data where
    type RecOf MCF1_Data = MCF1
    type DataOf MCF1 = MCF1_Data
    readData r = mcf1_Data r
    writeData r cs = r { mcf1_Data = cs }

instance Rec MCF1 where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- getList; return $ MCF1 a01 a02 a03 a04 a05
    recPut r = do put $ mcf1_Type r; put $ mcf1_ r; put $ mcf1_RepeatingGroupLength r; put $ mcf1__ r; putList $ mcf1_Data r; return ()
    recSizeOf r = sum [ sizeOf $ mcf1_Type r, sizeOf $ mcf1_ r, sizeOf $ mcf1_RepeatingGroupLength r, sizeOf $ mcf1__ r, sizeOf $ mcf1_Data r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mcf1_Type r), viewField "_" (viewNumber $ mcf1_ r), viewField "RepeatingGroupLength" (viewNumber $ mcf1_RepeatingGroupLength r), viewField "__" (viewString $ mcf1__ r), viewField "Data" (viewData $ mcf1_Data r) ]
    recType = fromEnum . mcf1_Type

instance Rec MDD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MDD a01 a02 a03
    recPut r = do put $ mdd_Type r; put $ mdd_ r; put $ mdd r; return ()
    recSizeOf r = sum [ sizeOf $ mdd_Type r, sizeOf $ mdd_ r, sizeOf $ mdd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mdd_Type r), viewField "_" (viewNumber $ mdd_ r), viewField "" (viewNStr $ mdd r) ]
    recType = fromEnum . mdd_Type

instance Rec MDR where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MDR a01 a02 a03
    recPut r = do put $ mdr_Type r; put $ mdr_ r; put $ mdr r; return ()
    recSizeOf r = sum [ sizeOf $ mdr_Type r, sizeOf $ mdr_ r, sizeOf $ mdr r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mdr_Type r), viewField "_" (viewNumber $ mdr_ r), viewField "" (viewNStr $ mdr r) ]
    recType = fromEnum . mdr_Type

instance Rec MFC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MFC a01 a02 a03
    recPut r = do put $ mfc_Type r; put $ mfc_ r; put $ mfc r; return ()
    recSizeOf r = sum [ sizeOf $ mfc_Type r, sizeOf $ mfc_ r, sizeOf $ mfc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mfc_Type r), viewField "_" (viewNumber $ mfc_ r), viewField "" (viewNStr $ mfc r) ]
    recType = fromEnum . mfc_Type

instance Rec MGO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MGO a01 a02 a03
    recPut r = do put $ mgo_Type r; put $ mgo_ r; put $ mgo r; return ()
    recSizeOf r = sum [ sizeOf $ mgo_Type r, sizeOf $ mgo_ r, sizeOf $ mgo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mgo_Type r), viewField "_" (viewNumber $ mgo_ r), viewField "" (viewNStr $ mgo r) ]
    recType = fromEnum . mgo_Type

instance Rec MIO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MIO a01 a02 a03
    recPut r = do put $ mio_Type r; put $ mio_ r; put $ mio r; return ()
    recSizeOf r = sum [ sizeOf $ mio_Type r, sizeOf $ mio_ r, sizeOf $ mio r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mio_Type r), viewField "_" (viewNumber $ mio_ r), viewField "" (viewNStr $ mio r) ]
    recType = fromEnum . mio_Type

instance Rec MMC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MMC a01 a02 a03
    recPut r = do put $ mmc_Type r; put $ mmc_ r; put $ mmc r; return ()
    recSizeOf r = sum [ sizeOf $ mmc_Type r, sizeOf $ mmc_ r, sizeOf $ mmc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mmc_Type r), viewField "_" (viewNumber $ mmc_ r), viewField "" (viewNStr $ mmc r) ]
    recType = fromEnum . mmc_Type

instance Rec MMO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MMO a01 a02 a03
    recPut r = do put $ mmo_Type r; put $ mmo_ r; put $ mmo r; return ()
    recSizeOf r = sum [ sizeOf $ mmo_Type r, sizeOf $ mmo_ r, sizeOf $ mmo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mmo_Type r), viewField "_" (viewNumber $ mmo_ r), viewField "" (viewNStr $ mmo r) ]
    recType = fromEnum . mmo_Type

instance Rec MMT where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MMT a01 a02 a03
    recPut r = do put $ mmt_Type r; put $ mmt_ r; put $ mmt r; return ()
    recSizeOf r = sum [ sizeOf $ mmt_Type r, sizeOf $ mmt_ r, sizeOf $ mmt r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mmt_Type r), viewField "_" (viewNumber $ mmt_ r), viewField "" (viewNStr $ mmt r) ]
    recType = fromEnum . mmt_Type

instance Rec MPG where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MPG a01 a02 a03
    recPut r = do put $ mpg_Type r; put $ mpg_ r; put $ mpg r; return ()
    recSizeOf r = sum [ sizeOf $ mpg_Type r, sizeOf $ mpg_ r, sizeOf $ mpg r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mpg_Type r), viewField "_" (viewNumber $ mpg_ r), viewField "" (viewNStr $ mpg r) ]
    recType = fromEnum . mpg_Type

instance Rec MPO where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MPO a01 a02 a03
    recPut r = do put $ mpo_Type r; put $ mpo_ r; put $ mpo r; return ()
    recSizeOf r = sum [ sizeOf $ mpo_Type r, sizeOf $ mpo_ r, sizeOf $ mpo r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mpo_Type r), viewField "_" (viewNumber $ mpo_ r), viewField "" (viewNStr $ mpo r) ]
    recType = fromEnum . mpo_Type

instance Rec MPS where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MPS a01 a02 a03
    recPut r = do put $ mps_Type r; put $ mps_ r; put $ mps r; return ()
    recSizeOf r = sum [ sizeOf $ mps_Type r, sizeOf $ mps_ r, sizeOf $ mps r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mps_Type r), viewField "_" (viewNumber $ mps_ r), viewField "" (viewNStr $ mps r) ]
    recType = fromEnum . mps_Type

instance Rec MSU where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MSU a01 a02 a03
    recPut r = do put $ msu_Type r; put $ msu_ r; put $ msu r; return ()
    recSizeOf r = sum [ sizeOf $ msu_Type r, sizeOf $ msu_ r, sizeOf $ msu r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ msu_Type r), viewField "_" (viewNumber $ msu_ r), viewField "" (viewNStr $ msu r) ]
    recType = fromEnum . msu_Type

instance Rec NOP where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ NOP a01 a02 a03
    recPut r = do put $ nop_Type r; put $ nop_ r; put $ nop r; return ()
    recSizeOf r = sum [ sizeOf $ nop_Type r, sizeOf $ nop_ r, sizeOf $ nop r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ nop_Type r), viewField "_" (viewNumber $ nop_ r), viewField "" (viewNStr $ nop r) ]
    recType = fromEnum . nop_Type

instance Rec OBD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ OBD a01 a02 a03
    recPut r = do put $ obd_Type r; put $ obd_ r; put $ obd r; return ()
    recSizeOf r = sum [ sizeOf $ obd_Type r, sizeOf $ obd_ r, sizeOf $ obd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ obd_Type r), viewField "_" (viewNumber $ obd_ r), viewField "" (viewNStr $ obd r) ]
    recType = fromEnum . obd_Type

instance Rec OBP where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ OBP a01 a02 a03
    recPut r = do put $ obp_Type r; put $ obp_ r; put $ obp r; return ()
    recSizeOf r = sum [ sizeOf $ obp_Type r, sizeOf $ obp_ r, sizeOf $ obp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ obp_Type r), viewField "_" (viewNumber $ obp_ r), viewField "" (viewNStr $ obp r) ]
    recType = fromEnum . obp_Type

instance Rec OCD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ OCD a01 a02 a03
    recPut r = do put $ ocd_Type r; put $ ocd_ r; put $ ocd r; return ()
    recSizeOf r = sum [ sizeOf $ ocd_Type r, sizeOf $ ocd_ r, sizeOf $ ocd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ocd_Type r), viewField "_" (viewNumber $ ocd_ r), viewField "" (viewNStr $ ocd r) ]
    recType = fromEnum . ocd_Type

instance Rec PFC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ PFC a01 a02 a03
    recPut r = do put $ pfc_Type r; put $ pfc_ r; put $ pfc r; return ()
    recSizeOf r = sum [ sizeOf $ pfc_Type r, sizeOf $ pfc_ r, sizeOf $ pfc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pfc_Type r), viewField "_" (viewNumber $ pfc_ r), viewField "" (viewNStr $ pfc r) ]
    recType = fromEnum . pfc_Type

instance Rec PGD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; a04 <- get; a05 <- get; a06 <- get; a07 <- get; a08 <- get; a09 <- get; a10 <- get; a11 <- get; return $ PGD a01 a02 a03 a04 a05 a06 a07 a08 a09 a10 a11
    recPut r = do put $ pgd_Type r; put $ pgd_ r; put $ pgd_XUnitBase r; put $ pgd_YUnitBase r; put $ pgd_XLUnitsperUnitBase r; put $ pgd_YLUnitsperUnitBase r; put $ pgd_Reserved1 r; put $ pgd_XPageSize r; put $ pgd_Reserved2 r; put $ pgd_YPageSize r; put $ pgd__ r; return ()
    recSizeOf r = sum [ sizeOf $ pgd_Type r, sizeOf $ pgd_ r, sizeOf $ pgd_XUnitBase r, sizeOf $ pgd_YUnitBase r, sizeOf $ pgd_XLUnitsperUnitBase r, sizeOf $ pgd_YLUnitsperUnitBase r, sizeOf $ pgd_Reserved1 r, sizeOf $ pgd_XPageSize r, sizeOf $ pgd_Reserved2 r, sizeOf $ pgd_YPageSize r, sizeOf $ pgd__ r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pgd_Type r), viewField "_" (viewNumber $ pgd_ r), viewField "XUnitBase" (viewNumber $ pgd_XUnitBase r), viewField "YUnitBase" (viewNumber $ pgd_YUnitBase r), viewField "XLUnitsperUnitBase" (viewNumber $ pgd_XLUnitsperUnitBase r), viewField "YLUnitsperUnitBase" (viewNumber $ pgd_YLUnitsperUnitBase r), viewField "Reserved1" (viewNumber $ pgd_Reserved1 r), viewField "XPageSize" (viewNumber $ pgd_XPageSize r), viewField "Reserved2" (viewNumber $ pgd_Reserved2 r), viewField "YPageSize" (viewNumber $ pgd_YPageSize r), viewField "__" (viewNStr $ pgd__ r) ]
    recType = fromEnum . pgd_Type

instance Rec PGP where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ PGP a01 a02 a03
    recPut r = do put $ pgp_Type r; put $ pgp_ r; put $ pgp r; return ()
    recSizeOf r = sum [ sizeOf $ pgp_Type r, sizeOf $ pgp_ r, sizeOf $ pgp r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pgp_Type r), viewField "_" (viewNumber $ pgp_ r), viewField "" (viewNStr $ pgp r) ]
    recType = fromEnum . pgp_Type

instance Rec PGP1 where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ PGP1 a01 a02 a03
    recPut r = do put $ pgp1_Type r; put $ pgp1_ r; put $ pgp1 r; return ()
    recSizeOf r = sum [ sizeOf $ pgp1_Type r, sizeOf $ pgp1_ r, sizeOf $ pgp1 r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pgp1_Type r), viewField "_" (viewNumber $ pgp1_ r), viewField "" (viewNStr $ pgp1 r) ]
    recType = fromEnum . pgp1_Type

instance Rec PMC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ PMC a01 a02 a03
    recPut r = do put $ pmc_Type r; put $ pmc_ r; put $ pmc r; return ()
    recSizeOf r = sum [ sizeOf $ pmc_Type r, sizeOf $ pmc_ r, sizeOf $ pmc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ pmc_Type r), viewField "_" (viewNumber $ pmc_ r), viewField "" (viewNStr $ pmc r) ]
    recType = fromEnum . pmc_Type

instance Rec PTD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ PTD a01 a02 a03
    recPut r = do put $ ptd_Type r; put $ ptd_ r; put $ ptd r; return ()
    recSizeOf r = sum [ sizeOf $ ptd_Type r, sizeOf $ ptd_ r, sizeOf $ ptd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptd_Type r), viewField "_" (viewNumber $ ptd_ r), viewField "" (viewNStr $ ptd r) ]
    recType = fromEnum . ptd_Type

instance Rec PTD1 where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ PTD1 a01 a02 a03
    recPut r = do put $ ptd1_Type r; put $ ptd1_ r; put $ ptd1 r; return ()
    recSizeOf r = sum [ sizeOf $ ptd1_Type r, sizeOf $ ptd1_ r, sizeOf $ ptd1 r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ ptd1_Type r), viewField "_" (viewNumber $ ptd1_ r), viewField "" (viewNStr $ ptd1 r) ]
    recType = fromEnum . ptd1_Type

instance RecChunk TLE where
    type ChunkOf TLE = T_
    readChunks r = tle_Chunks r
    writeChunks r io = do
        cs <- io
        return $ r { tle_Chunks = cs }

instance Rec TLE where
    recGet = do a01 <- get; a02 <- get; a03 <- getList; return $ TLE a01 a02 a03
    recPut r = do put $ tle_Type r; put $ tle_ r; putList $ tle_Chunks r
    recSizeOf r = sum [ sizeOf $ tle_Type r, sizeOf $ tle_ r, sizeOf $ tle_Chunks r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ tle_Type r), viewField "_" (viewNumber $ tle_ r), viewField "Chunks" (viewChunks $ tle_Chunks r) ]
    recType = fromEnum . tle_Type

instance RecChunk MCF_T where
    type ChunkOf MCF_T = T_
    readChunks r = mcf_t_Chunks r
    writeChunks r io = do
        cs <- io
        return $ r { mcf_t_Chunks = cs }

instance Rec MCF_T where
    recGet = do a01 <- getList; return $ MCF_T a01
    recPut r = do putList $ mcf_t_Chunks r; return ()
    recSizeOf r = sum [ sizeOf $ mcf_t_Chunks r ]
    recView r = viewRecord (typeOf r) [ viewField "Chunks" (viewChunks $ mcf_t_Chunks r) ]
    recType r = 0

