{-# OPTIONS -O -fglasgow-exts -funbox-strict-fields #-}

module Main where
import OpenAFP
import qualified Data.Map as Map
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Int

-- | Flags.
type VarsIO a = StateIO Vars a

-- | Global configuration parameters.
dbcsSpace, dbcsFirst :: NChar
dbcsSpace  = (0x40, 0x40)
dbcsFirst = (0x4C, 0x41)

isUDC :: N1 -> Bool
isUDC  hi = hi >= 0x92 && hi <= 0xFE

{-# NOINLINE _Vars #-}
_Vars :: IORef Vars
_Vars = unsafePerformIO $ newIORef undefined

-- | The main program: initialize global variables, split the input
-- AFP file into "pages", then dispatch each page to pageHandler.
main :: IO ()
main = do
    writeIORef _Vars =<< initVars
    stateMain

instance MonadReader Vars IO where
    ask = readIORef _Vars
    local = undefined

stateMain = do
    chunks  <- liftOpt readInputAFP
    fh      <- liftOpt openOutputAFP
    forM_ (splitRecords _PGD chunks) $ \cs -> do
        cs' <- pageHandler cs
        io $ L.hPutStr fh (encodeList cs')
    io $ hClose fh

-- | Check a page's PTX records for UDC characters.  If none is seen,
-- simply output the page; otherwise pass the page to udcPageHandler
-- and output the result.
pageHandler page = (`catchError` hdl) $ do
    page ..>
        [ _MCF  ... mcfHandler
        , _MCF1 ... mcf1Handler
        , _PTX  ... ptxCheckUDC
        ]
    return page
    where
    hdl err | Just e <- cast err, isUserError e = udcPageHandler page
            | otherwise = throwError err

-- | Record font Id to Name mappings in MCF's RLI and FQN chunks.
mcfHandler r = do
    readChunks r ..>
        [ _MCF_T ... \mcf -> do
            let cs = readChunks mcf
            ids   <- sequence [ t_rli `applyToChunk` c | c <- cs, c ~~ _T_RLI ]
            fonts <- sequence [ t_fqn `applyToChunk` c | c <- cs, c ~~ _T_FQN ]
            _IdToFont %%= (ids `zip` map packAStr fonts)
        ]

-- | Record font Id to Name mappings in MCF1's Data chunks.
mcf1Handler r = do
    _IdToFont %%=
        [ (mcf1_CodedFontLocalId mcf1, packA8 $ mcf1_CodedFontName mcf1)
            | Record mcf1 <- readData r
        ]

-- | Split PTX into "groups", each one begins with a SCFL chunk.
ptxCheckUDC r = length groups `seq` forM_ groups ptxGroupCheckUDC
    where
    groups = splitRecords _PTX_SCFL chunks
    chunks = filter want (readChunks r)
    want c = (c ~~ _PTX_SCFL) || (c ~~ _PTX_TRN) 

-- | Check a PTX to see if the leading SCFL indicates a DBCS font;
-- if yes, pass remaining TRN chunks to trnCheckUDC.
ptxGroupCheckUDC [] = return ()
ptxGroupCheckUDC (scfl:trnList) = seq (length trnList) $ do
    font    <- readArray _IdToFont =<< ptx_scfl `applyToChunk` scfl
    isDBCS  <- fontIsDBCS &: font
    when (isDBCS) . io $ do
        forM_ (map (ptx_trn `applyToChunk`) trnList)
            (>>= trnCheckUDC)

-- | Look inside TRN buffers for UDC characters.  If we find one,
-- raise an IO exception so the page handler can switch to UDC mode.
trnCheckUDC :: NStr -> IO ()
trnCheckUDC nstr = S.unsafeUseAsCStringLen (packBuf nstr) $ \(cstr, len) -> do
    forM_ [0, 2..len-1] $ \off -> do
        hi <- peekByteOff cstr off
        when (isUDC hi) $ do
            throwError (strMsg "Found UDC")

-- | The main handler for pages with UDC characters.
udcPageHandler page = do
    (_X     $= id) 0
    (_Y     $= id) 0
    (_UDC   $= id) []
    page ==>
        [ _PGD  ... _XPageSize $= pgd_XPageSize
        , _MCF  ... mcfHandler
        , _MCF1 ... mcf1Handler
        , _PTX  === \ptx -> do
            filterChunks ptx
                [ _PTX_AMI  ... _X            $= ptx_ami
                , _PTX_AMB  ... _Y            $= ptx_amb
                , _PTX_RMI  ... _X            += ptx_rmi
                , _PTX_RMB  ... _Y            += ptx_rmb
                , _PTX_SCFL ... _FontId       $= ptx_scfl
                , _PTX_SBI  ... _BaselineIncr $= ptx_sbi
                , _PTX_SIM  ... _InlineMargin $= ptx_sim
                , _PTX_STO  ... \sto -> do
                    _XOrientation $= ptx_sto_Orientation   $ sto
                    _YOrientation $= ptx_sto_WrapDirection $ sto
                , _PTX_BLN  ... \_ -> do
                    x' <- readVar _InlineMargin
                    y' <- readVar _BaselineIncr
                    (_X $= id) x'
                    (_Y += id) y'
                , _PTX_TRN  === trnHandler
                ]
        , _EMO  === endPageHandler
        , _EPG  === endPageHandler
        ]

trnHandler r = do
    font    <- currentFont
    let trn = packNStr $ ptx_trn r
    isDBCS  <- fontIsDBCS &: font
    case isDBCS of
        True -> do
            -- If font is double byte, transform the TRN with dbcsHandler.
            db      <- dbcsHandler font trn -- XXX
            push r{ ptx_trn = mkBuf db }
        False -> do
            -- If font is single byte, simply add each byte's increments.
            -- without parsing UDC.
            let f tot ch = do
                    inc <- incrementOf font (0x00, if ch == 0x00 then 0x40 else N1 ch)
                    return (tot + inc)
            inc <- foldM f 0 (S.unpack trn)
            (_X += id) inc
            push r

dbcsHandler :: FontField -> ByteString -> VarsIO ByteString
dbcsHandler font bs = liftM (S.pack . map fromN1) (dbcsHandler' font (map N1 (S.unpack bs)))

dbcsHandler' :: FontField -> [N1] -> VarsIO [N1]
dbcsHandler' _ []    = return []
dbcsHandler' _ [x]   = do
    warn $ "invalid DBCS sequence: " ++ show x
    return []
dbcsHandler' font (hi:lo:xs) = do
    (incChar, (hi', lo'))   <- dbcsCharHandler font (hi, lo)
    incr                    <- incrementOf font incChar
    (_X += id) incr
    rest                    <- dbcsHandler' font xs
    return (hi':lo':rest)

dbcsCharHandler font char@(hi, _)
    | isUDC hi = do
        x <- readVar _X
        y <- readVar _Y
        _UDC @= CharUDC
            { udcX = x
            , udcY = y
            , udcChar = char
            , udcFont = font
            }
        return (char, dbcsSpace)
    | otherwise = return (dbcsFirst, char)
    -- isNotDBCS hi = return (char, char)

endPageHandler r = do
    udcList <- readVar _UDC
    unless (null udcList) $ do
        xp  <- readVar _XPageSize
        xo  <- readVar _XOrientation
        yo  <- readVar _YOrientation
        forM_ (reverse udcList) $ \udc -> do
            infoMaybe <- fontInfoOf (udcFont udc) (udcChar udc)
            case infoMaybe of
                Just info | S.length (fromBuf0 $ fontBitmap info) > 0 -> do
                    push _BII
                    push _IOC
                        { ioc_XMap            = 0x03E8
                        , ioc_XOrientation    = xo
                        , ioc_YMap            = 0x03E8
                        , ioc_YOrientation    = yo
                        }
                    udcCharHandler xp yo udc info
                    push _EII
                _ -> return ()
    push r

udcCharHandler xp yo char info = do
    base <- adjustY &: fontBaseOffset info
    let (x, y) = orientate x' y'
        x' = udcX char + (fromIntegral $ fontASpace info)
        y' = udcY char - (fromIntegral $ base)
    push _IID
        { iid_Color           = 0x0008
        , iid_ConstantData1   = 0x000009600960000000000000
        , iid_ConstantData2   = 0x000000002D00
        , iid_XUnits          = fontResolution info
        , iid_YUnits          = fontResolution info
        }
    push _ICP
        { icp_XCellOffset = x
        , icp_XCellSize   = fontWidth info
        , icp_XFillSize   = fontWidth info
        , icp_YCellOffset = y
        , icp_YCellSize   = fontHeight info
        , icp_YFillSize   = fontHeight info
        }
    push _IRD {
        ird_ImageData = fontBitmap info
    }
    where
    orientate x y
        | (yo == 0x5a00)    = (xp - y, x)
        | otherwise         = (x, y)

currentFont = readArray _IdToFont =<< readVar _FontId

incrementOf x y = do
    cachedLibReader _IncrCache fillInc 0 x y
    where
    fillInc font char@(hi, _) f redo = do
        rv <- readIncrements font hi
        if rv then liftM (maybe 0 id) redo else do
            udcRef <- asks _UDC
            io $ do
                -- Remove this from the list of UDCs to gen image for
                putStrLn $ "Skipping unknown UDC: " ++ show char ++ ", font: " ++ show (ck_font f)
                modifyIORef udcRef (filter ((/= char) . udcChar))
            return 0

fontInfoOf x y = do
    cachedLibReader _InfoCache fillInfo (Just _FontInfo) x y
    where
    fillInfo font char key _ = do
        infoMaybe <- readFontInfo font char
        case infoMaybe of
            Just info -> do
                _InfoCache  %= (key, infoMaybe)
                _IncrCache  %= (key, fontIncrement info)
                return infoMaybe
            _ -> return Nothing

cachedLibReader :: (MonadIO m, MonadReader Vars m, MonadPlus m, MonadError e m, Show e, Typeable e) =>
                     (Vars -> HashTable CacheKey a)
                       -> (ByteString -> NChar -> CacheKey -> m (Maybe a) -> m a)
                         -> a -> FontField -> NChar -> m a
cachedLibReader cache filler fallback font char = do
    vars <- ask
    let val = io $ hashLookup (cache vars) key
        fillCache = filler font char key val
    val >>= maybe (fillCache `catchError` hdl) return
    where
    key = cacheKey font char
    hdl err | Just e <- cast err, isUserError e = {-# SCC "hdl" #-} do
        let keySeen = cacheKey font (0x00, 0x00)
        seen <- cache %? keySeen
        when (isNothing seen) $ do
            warn $ "font library does not exist: " ++ C.unpack font
        cache %= (key, fallback)
        cache %= (keySeen, fallback)
        -- sequence_ [ cache %= (cacheKey font (fst char, lo), fallback) | <- [0x00 .. 0xFF] ]
        return fallback
            | otherwise = throwError err

readIncrements font hi = do
    cfi <- (_CFI <~~) =<< readFontlibAFP &<< font
    case hi `matchRecordMaybe` cfi_Section $ cfi of
        Nothing     -> return False
        Just cfi'   -> do
            fni <- _FNI `readLibRecord` cfi_FontCharacterSetName $ cfi'
            cpi <- _CPI `readLibRecord` cfi_CodePageName $ cfi'
            let gcgToIncr = Map.fromList . map (pair . fromRecord) $ readData fni
                pair r    = (fni_GCGID r, fni_CharacterIncrement r)
                incr      = maybe 0 id . (`Map.lookup` gcgToIncr) . cpi_GCGID
            sequence_ [ _IncrCache %= (key r, incr r) | Record r <- readData cpi ]
            return True
     where
     key r = cacheKey font (hi, cpi_CodePoint r)

readFontInfo font char@(hi, _) = do
    cfi <- (_CFI <~~) =<< readFontlibAFP &<< font
    case (hi `matchRecordMaybe` cfi_Section) cfi of
        Nothing     -> return Nothing
        Just cfi'   -> liftM Just (cfiHandler char cfi')

cfiHandler char@(_, lo) cfi = do
    cpi_    <- _CPI `readLibRecord` cfi_CodePageName $ cfi
    case lo `matchRecordMaybe` cpi_CodePoint $ cpi_ of
        Nothing -> return _FontInfo
        Just c  -> run lo (cpi_GCGID c) cfi
    where
    run 0x40 0   x = return _FontInfo
    run _    0   x = cfiHandler (0x00, 0x40) x
    run _    gcg x = do
        io $ putStrLn ("Replacing UDC: " ++ show char)
        fcsHandler gcg x

fcsHandler gcg cfi = do
    fcs  <- readFontlibAFP &<< (fromFontField $ cfi_FontCharacterSetName cfi)
    fni_ <- _FNI <~~ fcs
    fnm_ <- _FNM <~~ fcs
    fnc  <- _FNC <~~ fcs
    let fni     = gcg `matchRecord` fni_GCGID $ fni_
        fnm     = fromRecord $ readData fnm_ `genericIndex` fni_FNMCount fni
        (w, h)  = (1 + fnm_Width fnm, 1 + fnm_Height fnm)
        bytes   = ((w + 7) `div` 8) * h
    fngs    <- sequence [ fng `applyToChunk` n | n <- fcs, n ~~ _FNG ]
    let bitmap  = subBufs fngs (fnm_Offset fnm) bytes

    vars    <- ask
    when (verbose (_Opts vars)) $ do
        let nstr = fromNStr bitmap
        showBitmap nstr (bytes `div` h)

    return FontInfo
        { fontIncrement   = fni_CharacterIncrement fni
        , fontAscend      = fni_AscendHeight fni
        , fontDescend     = fni_DescendDepth fni
        , fontASpace      = fni_ASpace fni
        , fontBSpace      = fni_BSpace fni
        , fontCSpace      = fni_CSpace fni
        , fontBaseOffset  = fni_BaseOffset fni
        , fontBitmap      = bitmap
        , fontWidth       = w
        , fontHeight      = h
        , fontResolution  = fnc_UnitXValue fnc
        }

readLibRecord t f r = t `seq` f `seq` r `seq` ((t <~~) =<< readFontlibAFP &<< (fromFontField $ f r))

-- | Data structures.
data CharUDC = CharUDC
    { udcX        :: !N2
    , udcY        :: !N2
    , udcChar     :: !NChar
    , udcFont     :: !FontField
    } deriving (Show)

data FontInfo = FontInfo
    { fontIncrement   :: !N2
    , fontAscend      :: !I2
    , fontDescend     :: !I2
    , fontASpace      :: !I2
    , fontBSpace      :: !N2
    , fontCSpace      :: !I2
    , fontBaseOffset  :: !I2
    , fontBitmap      :: !NStr
    , fontWidth       :: !N2
    , fontHeight      :: !N2
    , fontResolution  :: !N2
    } deriving (Show)

_FontInfo = FontInfo 0 0 0 0 0 0 0 _NStr 0 0 0

-- | Simple test case.
run :: IO ()
run = withArgs (split " " "-a -d NS.. -i tmp -o x -f /StreamEDP/fontlib") main

data Vars = Vars
    { _XPageSize      :: !(IORef N2)
    , _X              :: !(IORef N2)
    , _Y              :: !(IORef N2)
    , _XOrientation   :: !(IORef N2)
    , _YOrientation   :: !(IORef N2)
    , _InlineMargin   :: !(IORef N2)
    , _BaselineIncr   :: !(IORef N2)
    , _FontId         :: !(IORef N1)
    , _UDC            :: !(IORef [CharUDC])
    , _IdToFont       :: !(IOArray N1 FontField)
    , _InfoCache      :: !(HashTable CacheKey (Maybe FontInfo))
    , _IncrCache      :: !(HashTable CacheKey N2)
    , _Opts           :: !Opts
    }

type Config = Int

data CacheKey = MkCacheKey
    { ck_hash   :: !Int32
    , ck_font   :: !ByteString
    }
    deriving Eq

cacheKey :: ByteString -> NChar -> CacheKey
cacheKey font (hi, lo) = MkCacheKey 
    (hashString (chr (fromEnum hi) : chr (fromEnum lo) : C.unpack font))
    font

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
    idf <- newIOArray (0x00, 0xFF) C.empty
    ifc <- hashNew (==) ck_hash
    inc <- hashNew (==) ck_hash
    opt <- getOpts
    return $ Vars xp x y xo yo im bi fid udc idf ifc inc opt

getOpts = do
    args <- getArgs
    (optsIO, rest, errs) <- return . getOpt Permute options
                                $ if (null args) then ["-h"] else args
    let opts    = foldl (flip ($)) defaultOpts optsIO
        suffix  = fontlibSuffix opts
    showHelp opts
    unless (null rest) $ do
        warn $ "unrecognized arguments: `" ++ joinList " " rest ++ "'\n"
    paths <- filterM checkPath $ fontlibPaths opts
    when (null paths) $ do
        die $ "cannot find a valid font library path"
    fontIsDBCS opts `seq` forM_ errs warn
    return opts{ readFontlibAFP = reader paths (fontlibSuffix opts) }
    where
    checkPath path = do
        exists <- doesDirectoryExist (C.unpack path)
        unless (exists) $ do
            warn $ "directory does not exist: `" ++ C.unpack path ++ "'"
        return exists
    reader paths suffix name = do
        -- Here we memoize by name.
        let key = C.concat (suffix:name:paths)
        rv <- hashLookup _ReaderCache key
        case rv of
            Just chunks -> return chunks
            _           -> do
                (afp:_) <- filterM doesFileExist
                    [ C.unpack f ++ "/" ++ fileName ++ C.unpack suffix | f <- paths ]
                chunks  <- readAFP afp
                hashInsert _ReaderCache key (seq (length chunks) chunks)
                return chunks
        where
        fileName = case C.unpack name of
            ('X':_:xs)  -> trim ('X':'0':xs)
            xs          -> trim xs

{-# NOINLINE _ReaderCache #-}
_ReaderCache :: HashTable ByteString [AFP_]
_ReaderCache = unsafePerformIO (hashNew (==) hashByteString)

infixl 4 &:
l &: v = do
    vars <- ask
    return $ l (_Opts vars) v

infixl 4 &<<
l &<< v = do
    vars <- ask
    io $ l (_Opts vars) v

liftOpt l = do
    vars <- ask
    io $ l (_Opts vars)


defaultOpts = Opts
    { adjustY           = id
    , fontIsDBCS        = const $ requiredOpt usage "dbcs-pattern"
    , dbcsPattern       = Nothing
    , readInputAFP      = fail $ requiredOpt usage "input"
    , openOutputAFP     = fail $ requiredOpt usage "output"
    , readFontlibAFP    = const $ error "fontlib" -- calculated with the two fields below
    , fontlibPaths      = [C.pack "fontlib"]
    , fontlibSuffix     = C.empty
    , cstrlenCheckUDC   = checkUDC
    , trnToSegments     = dbcsHandler
    , verbose           = False
    , showHelp          = return ()
    }
    where
    checkUDC (cstr, len) = forM_ [0, 2..len-1] $ \off -> do
        hi <- peekByteOff cstr off
        when (isUDC hi) $ fail "UDC"

usage :: String -> IO a
usage = showUsage options showInfo

showInfo prg = 
    "Usage: " ++ prg ++ " -i FILE -o FILE\n" ++
    "( example: " ++ prg ++ " -a -s .afp -d M..T -i in.afp -o out.afp )"

options :: [OptDescr (Opts -> Opts)]
options =
    [ noArg  "a" ["adjust"]                         "Adjust baseline offset"
        (\o   -> o { adjustY        = \y -> (y * 13) `div` 16 })
    , reqArg "c" ["codepage"]       "835|947"       "UDC codepage (default: 835)"
        setCodepage
    , reqArg "d" ["dbcs-pattern"]   "REGEXP"        "DBCS font name pattern"
        (\s o -> o { fontIsDBCS     = setMatchDBCS (C.pack s)
                   , dbcsPattern    = Just (C.pack s) })
    , reqArg "f" ["fontlib-path"]   "PATH,PATH..."  "Paths to font libraries"
        (\s o -> o { fontlibPaths   = C.split ',' (C.pack s) })
    , reqArg "i" ["input"]          "FILE"          "Input AFP file"
        (\s o -> o { readInputAFP   = readAFP s })
    , reqArg "o" ["output"]         "FILE"          "Output AFP file"
        (\s o -> o { openOutputAFP  = openBinaryFile s WriteMode })
    , reqArg "s" ["fontlib-suffix"] ".SUFFIX"       "Font library file suffix"
        (\s o -> o { fontlibSuffix  = C.pack s })
    , noArg  "v" ["verbose"]                        "Print progress information"
        (\o   -> o { verbose        = True })
    , noArg  "h" ["help"]                           "Show help"
        (\o   -> o { showHelp       = usage "" })
    ]
    where
    _M__T = C.pack "M..T"
    _NS__ = C.pack "NS.."
    _NS   = C.pack "NS"
    setMatchDBCS s a
        | s == _M__T = (C.length a >= 6) && ((C.index a 2 == 'M') || (C.index a 5 == 'T'))
        | s == _NS__ = (C.length a >= 6) && ((C.index a 2 == 'N') || (C.index a 3 == 'S'))
        | s == _NS   = (C.length a >= 4) && ((C.index a 2 == 'N') || (C.index a 3 == 'S'))
        | otherwise  = (C.unpack a =~ C.unpack s)
    setCodepage "835"  = id
    setCodepage "947"  = \o -> o
        { cstrlenCheckUDC   = checkUDC
        , trnToSegments     = segment
        -- if dbcsPattern is not set, set fontIsDBCS to (const True).
        , fontIsDBCS        = maybe (const True)
                                    (const . fontIsDBCS $ o)
                                    $ dbcsPattern o
        }
    setCodepage s      = unsafePerformIO $ do
        usage $ "invalid codepage:" ++ s
    checkUDC cstrlen = do
        str <- peekCStringLen cstrlen
        unless (str =~ noUDC) $ fail "UDC"
    noUDC = "^([\x00-\x7f]+|([\xA1-\xC5\xC9-\xF9].)+|(\xC6[^\xA1-\xFE])+)$"
    segment = error "segment"

type NChar = (N1, N1)
type FontField = ByteString
fromFontField = packA8

data Segment
    = SegmentUDC
        { segmentChar       :: !NChar
        , segmentReplace    :: !NChar
        }
    | SegmentDBCS
        { segmentChar       :: !NChar
        , segmentLength     :: Int
        }
    | SegmentByte
        { segmentByte       :: [N1]
        }
    deriving (Show, Typeable)

data Opts = Opts
    { fontIsDBCS        :: !(FontField -> Bool)
    , dbcsPattern       :: !(Maybe ByteString)
    , adjustY           :: !(I2 -> I2)
    , readInputAFP      :: !(IO [AFP_])
    , openOutputAFP     :: !(IO Handle)
    , readFontlibAFP    :: !(ByteString -> IO [AFP_])
    , fontlibPaths      :: !([ByteString])
    , fontlibSuffix     :: !(ByteString)
    , cstrlenCheckUDC   :: !(CStringLen -> IO ())
    , trnToSegments     :: FontField -> ByteString -> VarsIO ByteString
    , verbose           :: !(Bool)
    , showHelp          :: !(IO ())
    } deriving (Typeable)
