{-# OPTIONS -O -fglasgow-exts -funbox-strict-fields #-}

module Main where
import OpenAFP.Prelude

-- | Global configuration parameters.
_Adjust :: Bool
_Adjust = True

isUDC, isDBCS :: N1 -> Bool
isUDC  hi = hi >= 0x92 && hi <= 0xFE
isDBCS hi = hi >= 0x40 && hi <= 0x91

fillChar, firstChar :: [N1]
fillChar  = [0x40, 0x40]
firstChar = [0x4C, 0x41]

fontIsDBCS :: String -> Bool
fontIsDBCS (_:_:'M':_:_:'T':_)  = True
fontIsDBCS _                    = False

type StateIO a = (MonadIO m, MonadReader Vars m, MonadError e m) => m a
type WriterStateIO a = (Chunk c, MonadReader Vars m, MonadIO m, MonadError e m) => ChunkWriter c m a
type AFP_WriterStateIO = ChunkWriter AFP_ (ReaderT Vars IO)
type PTX_WriterStateIO = ChunkWriter PTX_ AFP_WriterStateIO

-- | The main program: initialize global variables, split the input
-- AFP file into "pages", then dispatch each page to pageHandler.
-- Recover gracefully from IO errors.
main :: IO ()
main = do
    (input : output : _) <- readArgs 2
    chunks  <- readAFP input
    fh      <- openBinaryFile output WriteMode
    bh      <- openBinIO_ fh
    vars    <- initVars
    runReaderT (mapM_ (pageHandler bh) $ splitRecords _PGD chunks) vars
    hClose fh

pageHandler :: BinHandle -> [AFP_] -> ReaderT Vars IO ()
pageHandler bh page = do
    page' <- pageHasUDC page `catchError` hdl
    liftIO $ put bh page'
    return ()
    where
    hdl err | isUserError err = udcPageHandler page
            | otherwise       = throwError err

pageHasUDC :: [AFP_] -> StateIO [AFP_]
pageHasUDC page = do
    page ..> [ _MCF ... mcfHandler, _PTX ... ptxHasUDC ]
    return page

mcfHandler :: MCF -> StateIO ()
mcfHandler r = readChunks r ..> [
    _MCF_T ... \mcf -> do
        let cs = readChunks mcf
        ids   <- sequence [ t_rli `applyToChunk` c | c <- cs, c ~~ _T_RLI ]
        fonts <- sequence [ t_fqn `applyToChunk` c | c <- cs, c ~~ _T_FQN ]
        _IdToFont %%= ids `zip` fonts
    ]

-- | Splits PTX into a number of "groups"; each one begins with a SCFL chunk,
-- followed by any number of TRN chunks.
ptxHasUDC :: PTX -> StateIO ()
ptxHasUDC r = readChunks r ..> [
    _PTX_SCFL ... _FontId $= ptx_scfl,
    _PTX_TRN  ... trnHasUDC . ptx_trn
    ]

currentFont :: StateIO String
currentFont = do
    id      <- readVar _FontId
    fnt     <- readArray _IdToFont id
    str     <- fromAStr $ fnt
    return str

-- | Look inside TRN buffers for UDC characters.  If one is found,
-- raise an IO exception so the page handler can switch to UDC mode.
trnHasUDC :: NStr -> StateIO ()
trnHasUDC nstr = do
    font    <- currentFont
    when (fontIsDBCS font) $ liftIO $ do
        withForeignPtr (castForeignPtr pstr) $ \cstr ->
            mapM_ (\off -> do
                hi <- peekByteOff cstr off
                when (isUDC hi) $
                    throwError $ strMsg "Found UDC")
                $ offsets
    where
    (pstr, len) = bufToPStrLen nstr
    offsets = [0, 2..len-1]

-- | The main handler for pages with UDC characters.
udcPageHandler :: [AFP_] -> ReaderT Vars IO [AFP_]
udcPageHandler page = do
    (_X     $= id) 0
    (_Y     $= id) 0
    (_UDC   $= id) []
    page ==> [
        _PGD ... _XPageSize $= pgd_XPageSize,
        _MCF ... mcfHandler,
        _EMO === endPageHandler,
        _EPG === endPageHandler,
        _PTX === \r -> do
            filterChunks r [
                _PTX_AMI  ... _X            $= ptx_ami,
                _PTX_AMB  ... _Y            $= ptx_amb,
                _PTX_RMI  ... _X            += ptx_rmi,
                _PTX_RMB  ... _Y            += ptx_rmb,
                _PTX_SCFL ... _FontId       $= (fromIntegral . ptx_scfl),
                _PTX_SBI  ... _BaselineIncr $= ptx_sbi,
                _PTX_SIM  ... _InlineMargin $= ptx_sim,
                _PTX_STO  ... \r -> do
                    _XOrientation $= ptx_sto_Orientation   $ r
                    _YOrientation $= ptx_sto_WrapDirection $ r
                ,
                _PTX_BLN  ... \_ -> do
                    x' <- readVar _InlineMargin
                    y' <- readVar _BaselineIncr
                    (_X $= id) x'
                    (_Y += id) y'
                ,
                _PTX_TRN  === trnHandler
                ]
        ]

trnHandler :: PTX_TRN -> PTX_WriterStateIO ()
trnHandler r = do
    font    <- currentFont
    trn     <- fromNStr $ ptx_trn r
    if (fontIsDBCS font) then do
        db      <- dbcsHandler font trn
        nstr    <- toNStr db
        push r { ptx_trn = nstr }
        else do
        -- If font is single byte, simply add increments
        -- together without parsing UDC
        incrs   <- mapM (\r -> incrementOf font [0x00, r]) trn
        (_X += sum) incrs
        push r

dbcsHandler :: String -> [N1] -> PTX_WriterStateIO [N1]
dbcsHandler _ [] = return []
dbcsHandler font (hi:lo:xs) = do
    (incChar, char) <- dbcsCharHandler font [hi, lo]
    incr            <- incrementOf font incChar
    (_X += id) incr
    rest            <- dbcsHandler font xs
    return $ char ++ rest

dbcsCharHandler :: String -> [N1] -> PTX_WriterStateIO ([N1], [N1])
dbcsCharHandler font char@(hi:_)
    | isUDC hi = do
        x <- readVar _X
        y <- readVar _Y
        _UDC @= CharUDC {
            udcX = x,
            udcY = y,
            udcChar = char,
            udcFont = font
        }
        return (char, fillChar)
    | isDBCS hi = return (firstChar, char)
    | otherwise = return (char, char)

endPageHandler :: (Rec r) => r -> AFP_WriterStateIO ()
endPageHandler r = do
    udcList <- readVar _UDC
    udcHandler udcList
    push r

udcHandler :: [CharUDC] -> AFP_WriterStateIO ()
udcHandler []  = return ()
udcHandler udcList = do
    xp  <- readVar _XPageSize
    xo  <- readVar _XOrientation
    yo  <- readVar _YOrientation
    push _BII
    push _IOC {
        ioc_XMap            = 0x03E8,
        ioc_XOrientation    = xo,
        ioc_YMap            = 0x03E8,
        ioc_YOrientation    = yo
    }
    mapM_ (udcCharHandler xp yo)
        $ reverse udcList
    push _EII

udcCharHandler :: N2 -> N2 -> CharUDC -> AFP_WriterStateIO ()
udcCharHandler xp yo char = do
    info <- fontInfoOf (udcFont char) (udcChar char)
    let (x, y) = orientate x' y'
        x' = udcX char + adjustX (fromIntegral $ fontASpace info)
        y' = udcY char - adjustY (fromIntegral $ fontBaseOffset info)
    push _IID {
        iid_Color           = 0x0008,
        iid_ConstantData1   = 0x000009600960000000000000,
        iid_ConstantData2   = 0x000000002D00,
        iid_XUnits          = fontResolution info,
        iid_YUnits          = fontResolution info
    }
    push _ICP {
        icp_XCellOffset = x,
        icp_XCellSize   = fontWidth info,
        icp_XFillSize   = fontWidth info,
        icp_YCellOffset = y,
        icp_YCellSize   = fontHeight info,
        icp_YFillSize   = fontHeight info
    }
    push _IRD {
        ird_ImageData = fontBitmap info
    }
    where
    orientate x y
        | (yo == 0x5a00)    = (xp - y, x)
        | otherwise         = (x, y)
    adjustX = id
    adjustY n
        | _Adjust   = (n * 13) `div` 16
        | otherwise = n

incrementOf :: String -> [N1] -> WriterStateIO N2
incrementOf = cachedLibReader _IncrCache fillInc 0
    where
    fillInc font (hi:_) _ redo = do
        readIncrements font hi
        redo

fontInfoOf :: String -> [N1] -> WriterStateIO FontInfo
fontInfoOf = cachedLibReader _InfoCache fillInfo _FontInfo
    where
    fillInfo font char key _ = do
        info <- readFontInfo font char
        _InfoCache  %= (key, info)
        _IncrCache  %= (key, fontIncrement info)
        return info

cachedLibReader :: (MonadIO m, MonadReader Vars m, MonadError e m) => (Vars -> HashTable String a)
                   -> (String -> [N1] -> String -> m a -> m a)
                      -> a -> String -> [N1] -> m a
cachedLibReader cache filler fallback font' char = do
    vars <- ask
    let val = liftIO $ hashLookup (cache vars) key
        fillCache = filler font char key (return . fromJust =<< val)
    val >>= maybe (fillCache `catchError` hdl) return
    where
    key = cacheKey font char
    font = trim $ "X0" ++ drop 2 font'
    hdl err = do
        -- unless (isDoesNotExistError err) $ throwError err
        liftIO $ print $ "*** Font library does not exist: " ++ font
        cache %= (key, fallback)
        return fallback

readIncrements :: (MonadIO m, MonadReader Vars m) => String -> N1 -> m ()
readIncrements font hi = do
    cfi_    <- (_CFI <~~) =<< readLibAFP font
    let cfi = hi `matchRecord` cfi_Section $ cfi_
    fni_    <- _FNI `readLibRecord` cfi_FontCharacterSetName $ cfi
    cpi_    <- _CPI `readLibRecord` cfi_CodePageName $ cfi
    let gcgToIncr = listToFM . map (pair . fromRecord) $ readData fni_
        pair r    = (fni_GCGID r, fni_CharacterIncrement r)
        incr      = fromJust . lookupFM gcgToIncr . cpi_GCGID
    sequence_ [ _IncrCache %= (key r, incr r) | Record r <- readData cpi_ ]
    return ()
    where
    key r = cacheKey font [hi, cpi_CodePoint r]

readFontInfo :: String -> [N1] -> WriterStateIO FontInfo
readFontInfo font char@(hi:_) = do
    cfi_ <- (_CFI <~~) =<< readLibAFP font
    cfiHandler char $ (hi `matchRecord` cfi_Section) cfi_

cfiHandler :: [N1] -> CFI_Data -> WriterStateIO FontInfo
cfiHandler (_:lo:[]) cfi = do
    cpi_    <- _CPI `readLibRecord` cfi_CodePageName $ cfi
    let gcg = cpi_GCGID $ lo `matchRecord` cpi_CodePoint $ cpi_
    run lo gcg cfi
    where
    run 0x40 0   = const $ return _FontInfo
    run _    0   = cfiHandler [0x00, 0x40]
    run _    gcg = fcsHandler gcg

fcsHandler :: A8 -> CFI_Data -> WriterStateIO FontInfo
fcsHandler gcg cfi = do
    fcs  <- readLibAFP =<< toAsc (cfi_FontCharacterSetName cfi)
    fni_ <- _FNI <~~ fcs
    fnm_ <- _FNM <~~ fcs
    fnc  <- _FNC <~~ fcs
    let fni     = gcg `matchRecord` fni_GCGID $ fni_
        fnm     = fromRecord $ readData fnm_ `genericIndex` fni_FNMCount fni
        (w, h)  = (1 + fnm_Width fnm, 1 + fnm_Height fnm)
        bytes   = ((w + 7) `div` 8) * h
    fngs <- sequence [ fng `applyToChunk` n | n <- fcs, n ~~ _FNG ]
    bitmap <- subBufs fngs (fnm_Offset fnm) bytes
    -- nstr <- fromNStr bitmap
    -- showBitmap nstr (bytes `div` h)
    return FontInfo {
        fontIncrement   = fni_CharacterIncrement fni,
        fontAscend      = fni_AscendHeight fni,
        fontDescend     = fni_DescendDepth fni,
        fontASpace      = fni_ASpace fni,
        fontBSpace      = fni_BSpace fni,
        fontCSpace      = fni_CSpace fni,
        fontBaseOffset  = fni_BaseOffset fni,
        fontBitmap      = bitmap,
        fontWidth       = w,
        fontHeight      = h,
        fontResolution  = fnc_UnitXValue fnc
    }

readLibAFP :: (MonadIO m) => String -> m [AFP_]
readLibAFP name = readAFP $ "fontlib/" ++ name ++ ".afp"

readLibRecord :: (MonadIO m, Storable b, Binary b, Rec c, Typeable t) =>
                    t -> (a -> b) -> a -> m c
readLibRecord t f = ((t <~~) =<<) . (readLibAFP =<<) . toAsc . f

-- | Data structures.
data CharUDC = CharUDC {
    udcX        :: !N2,
    udcY        :: !N2,
    udcChar     :: ![N1],
    udcFont     :: !String
} deriving (Show)

data FontInfo = FontInfo {
    fontIncrement   :: !N2,
    fontAscend      :: !I2,
    fontDescend     :: !I2,
    fontASpace      :: !I2,
    fontBSpace      :: !N2,
    fontCSpace      :: !I2,
    fontBaseOffset  :: !I2,
    fontBitmap      :: !NStr,
    fontWidth       :: !N2,
    fontHeight      :: !N2,
    fontResolution  :: !N2
} deriving (Show)

_FontInfo = FontInfo 0 0 0 0 0 0 0 _NStr 0 0 0

-- | Simple test case.
run :: IO ()
run = withArgs ["ln-1.afp", "fixed-1.afp"] main

data Vars = Vars {
    _XPageSize      :: !(IORef N2),
    _X              :: !(IORef N2),
    _Y              :: !(IORef N2),
    _XOrientation   :: !(IORef N2),
    _YOrientation   :: !(IORef N2),
    _InlineMargin   :: !(IORef N2),
    _BaselineIncr   :: !(IORef N2),
    _FontId         :: !(IORef N1),
    _UDC            :: !(IORef [CharUDC]),
    _IdToFont       :: !(IOArray N1 NStr),
    _InfoCache      :: !(HashTable String FontInfo),
    _IncrCache      :: !(HashTable String N2)
}

cacheKey :: String -> [N1] -> String
cacheKey font char = (font ++ map (chr . fromEnum) char)

initVars :: IO Vars
initVars = do
    xp  <- newIORef 0
    x   <- newIORef 0
    y   <- newIORef 0
    xo  <- newIORef 0
    yo  <- newIORef 0
    im  <- newIORef 0
    bi  <- newIORef 0
    fid <- newIORef 0
    udc <- newIORef []
    idf <- newIOArray (0x00, 0xFF) _NStr
    ifc <- hashNew (==) hashString
    inc <- hashNew (==) hashString
    return $ Vars xp x y xo yo im bi fid udc idf ifc inc
