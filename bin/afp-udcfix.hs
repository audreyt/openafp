{-# OPTIONS -O -fglasgow-exts -funbox-strict-fields #-}

module Main where
import OpenAFP
import qualified Data.Map as Map

-- | Flags.
type VarsIO a = StateIO Vars a
type WriterVarsIO a = WriterStateIO Vars a

-- | Global configuration parameters.
dbcsSpace, dbcsFirst :: NChar
dbcsSpace  = (0x40, 0x40)
dbcsFirst = (0x4C, 0x41)

isUDC :: N1 -> Bool
isUDC  hi = hi >= 0x92 && hi <= 0xFE

-- | The main program: initialize global variables, split the input
-- AFP file into "pages", then dispatch each page to pageHandler.
main :: IO ()
main = runReaderT stateMain =<< initVars

stateMain :: ReaderT Vars IO ()
stateMain = do
    chunks  <- liftOpt readInputAFP
    fh      <- liftOpt openOutputAFP
    bh      <- liftIO $ openBinIO_ fh
    forM_ (splitRecords _PGD chunks)
          ((liftIO . put bh =<<) . pageHandler)
    liftIO $ hClose fh

-- | Check a page's PTX records for UDC characters.  If none is seen,
-- simply output the page; otherwise pass the page to udcPageHandler
-- and output the result.
pageHandler :: [AFP_] -> VarsIO [AFP_]
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
mcfHandler :: MCF -> VarsIO ()
mcfHandler r = do
    readChunks r ..>
        [ _MCF_T ... \mcf -> do
            let cs = readChunks mcf
            ids   <- sequence [ t_rli `applyToChunk` c | c <- cs, c ~~ _T_RLI ]
            fonts <- sequence [ t_fqn `applyToChunk` c | c <- cs, c ~~ _T_FQN ]
            _IdToFont %%= ids `zip` map fromA8 fonts
        ]

-- | Record font Id to Name mappings in MCF1's Data chunks.
mcf1Handler :: MCF1 -> VarsIO ()
mcf1Handler r = do
    _IdToFont %%=
        [ (mcf1_CodedFontLocalId r, fromA8 $ mcf1_CodedFontName r)
            | Record r <- readData r
        ]

-- | Split PTX into "groups", each one begins with a SCFL chunk.
ptxCheckUDC :: PTX -> VarsIO ()
ptxCheckUDC r = forM_ groups ptxGroupCheckUDC
    where
    groups = splitRecords _PTX_SCFL chunks
    chunks = filter want (readChunks r)
    want c = (c ~~ _PTX_SCFL) || (c ~~ _PTX_TRN) 

-- | Check a PTX to see if the leading SCFL indicates a DBCS font;
-- if yes, pass remaining TRN chunks to trnCheckUDC.
ptxGroupCheckUDC :: [PTX_] -> VarsIO ()
ptxGroupCheckUDC (scfl:trnList) = do
    font    <- readArray _IdToFont =<< ptx_scfl `applyToChunk` scfl
    isDBCS  <- fontIsDBCS &: font
    when (isDBCS) $ liftIO $ do
        forM_ (map (ptx_trn `applyToChunk`) trnList)
            (>>= trnCheckUDC)

-- | Look inside TRN buffers for UDC characters.  If we find one,
-- raise an IO exception so the page handler can switch to UDC mode.
trnCheckUDC :: NStr -> IO ()
trnCheckUDC nstr = do
    withForeignPtr (castForeignPtr pstr) $ \cstr -> do
        forM_ offsets $ \off -> do
            hi <- peekByteOff cstr off
            when (isUDC hi) $ do
                throwError (strMsg "Found UDC")
    where
    (pstr, len) = bufToPStrLen nstr
    offsets = [0, 2..len-1]

-- | The main handler for pages with UDC characters.
udcPageHandler :: [AFP_] -> VarsIO [AFP_]
udcPageHandler page = do
    (_X     $= id) 0
    (_Y     $= id) 0
    (_UDC   $= id) []
    page ==>
        [ _PGD  ... _XPageSize $= pgd_XPageSize
        , _MCF  ... mcfHandler
        , _MCF1 ... mcf1Handler
        , _PTX  === \r -> do
            filterChunks r
                [ _PTX_AMI  ... _X            $= ptx_ami
                , _PTX_AMB  ... _Y            $= ptx_amb
                , _PTX_RMI  ... _X            += ptx_rmi
                , _PTX_RMB  ... _Y            += ptx_rmb
                , _PTX_SCFL ... _FontId       $= ptx_scfl
                , _PTX_SBI  ... _BaselineIncr $= ptx_sbi
                , _PTX_SIM  ... _InlineMargin $= ptx_sim
                , _PTX_STO  ... \r -> do
                    _XOrientation $= ptx_sto_Orientation   $ r
                    _YOrientation $= ptx_sto_WrapDirection $ r
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

trnHandler :: PTX_TRN -> WriterVarsIO ()
trnHandler r = do
    font    <- currentFont
    trn     <- fromNStr $ ptx_trn r
    isDBCS  <- fontIsDBCS &: font
    case isDBCS of
        True -> do
            -- If font is double byte, transform the TRN with dbcsHandler.
            vars <- ask
            db      <- trnToSegments (_Opts vars) font trn -- XXX
            nstr    <- toNStr db
            push r { ptx_trn = nstr }
        False -> do
            -- If font is single byte, simply add each byte's increments.
            -- without parsing UDC.
            incrs   <- forM trn $ \r -> do
                incrementOf font (0x00, r)
            (_X += sum) incrs
            push r

dbcsHandler :: FontField -> [N1] -> VarsIO [N1]
dbcsHandler _ [] = return []
dbcsHandler font (hi:lo:xs) = do
    (incChar, (hi', lo'))   <- dbcsCharHandler font (hi, lo)
    incr                    <- incrementOf font incChar
    (_X += id) incr
    rest                    <- dbcsHandler font xs
    return (hi':lo':rest)

dbcsCharHandler :: FontField -> NChar -> VarsIO (NChar, NChar)
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

endPageHandler :: (Rec r) => r -> WriterVarsIO ()
endPageHandler r = do
    udcList <- readVar _UDC
    unless (null udcList) $ do
        xp  <- readVar _XPageSize
        xo  <- readVar _XOrientation
        yo  <- readVar _YOrientation
        push _BII
        push _IOC
            { ioc_XMap            = 0x03E8
            , ioc_XOrientation    = xo
            , ioc_YMap            = 0x03E8
            , ioc_YOrientation    = yo
            }
        forM_ (reverse udcList) (udcCharHandler xp yo)
        push _EII
    push r

udcCharHandler :: N2 -> N2 -> CharUDC -> WriterVarsIO ()
udcCharHandler xp yo char = do
    (verbose . _Opts) $$ ("Found: " ++ show char)
    info <- fontInfoOf (udcFont char) (udcChar char)
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

currentFont :: VarsIO FontField
currentFont = readArray _IdToFont =<< readVar _FontId

incrementOf :: FontField -> NChar -> VarsIO N2
incrementOf x y = do
    cachedLibReader _IncrCache fillInc 0 x y
    where
    fillInc font (hi, _) _ redo = do
        readIncrements font hi
        redo

fontInfoOf :: FontField -> NChar -> VarsIO FontInfo
fontInfoOf x y = do
    cachedLibReader _InfoCache fillInfo _FontInfo x y
    where
    fillInfo font char key _ = do
        info <- readFontInfo font char
        _InfoCache  %= (key, info)
        _IncrCache  %= (key, fontIncrement info)
        return info

cachedLibReader :: (MonadIO m, MonadReader Vars m, MonadPlus m, MonadError e m, Show e, Typeable e) =>
                     (Vars -> HashTable String a)
                       -> (String -> NChar -> String -> m a -> m a)
                         -> a -> FontField -> NChar -> m a
cachedLibReader cache filler fallback font char = do
    vars <- ask
    let val = liftIO $ hashLookup (cache vars) key
        fillCache = filler fontstr char key (return . fromJust =<< val)
    val >>= maybe (fillCache `catchError` hdl) return
    where
    key = cacheKey fontstr char
    font' = font
    fontstr | ('X':_:xs) <- font'  = trim ('X':'0':xs)
            | otherwise            = trim font'
    hdl err | Just e <- cast err, isUserError e = do
        let keySeen = cacheKey fontstr (0x00, 0x00)
        seen <- cache %? keySeen
        when (isNothing seen) $ do
            warn $ "font library does not exist: " ++ fontstr
        cache %= (key, fallback)
        cache %= (keySeen, fallback)
        -- sequence_ [ cache %= (cacheKey fontstr (fst char, lo), fallback) | <- [0x00 .. 0xFF] ]
        return fallback
            | otherwise = throwError err

readIncrements :: String -> N1 -> VarsIO ()
readIncrements font hi = do
    cfi_    <- (_CFI <~~) =<< readFontlibAFP &<< font
    let cfi = hi `matchRecord` cfi_Section $ cfi_
    fni_    <- _FNI `readLibRecord` cfi_FontCharacterSetName $ cfi
    cpi_    <- _CPI `readLibRecord` cfi_CodePageName $ cfi
    let gcgToIncr = Map.fromList . map (pair . fromRecord) $ readData fni_
        pair r    = (fni_GCGID r, fni_CharacterIncrement r)
        incr      = fromJust . (`Map.lookup` gcgToIncr) . cpi_GCGID
    sequence_ [ _IncrCache %= (key r, incr r) | Record r <- readData cpi_ ]
    where
    key r = cacheKey font (hi, cpi_CodePoint r)

readFontInfo :: String -> NChar -> VarsIO FontInfo
readFontInfo font char@(hi, _) = do
    cfi_ <- (_CFI <~~) =<< readFontlibAFP &<< font
    cfiHandler char $ (hi `matchRecord` cfi_Section) cfi_

cfiHandler :: NChar -> CFI_Data -> VarsIO FontInfo
cfiHandler (_, lo) cfi = do
    cpi_    <- _CPI `readLibRecord` cfi_CodePageName $ cfi
    let gcg = cpi_GCGID $ lo `matchRecord` cpi_CodePoint $ cpi_
    run lo gcg cfi
    where
    run 0x40 0   x = return _FontInfo
    run _    0   x = cfiHandler (0x00, 0x40) x
    run _    gcg x = fcsHandler gcg x

fcsHandler :: A8 -> CFI_Data -> VarsIO FontInfo
fcsHandler gcg cfi = do
    fcs  <- readFontlibAFP &<< (fromFontField $ cfi_FontCharacterSetName cfi)
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

readLibRecord :: (Show t, Typeable t, Rec b) => t -> (a -> A8) -> a -> VarsIO b
readLibRecord t f r = (t <~~) =<< readFontlibAFP &<< (fromFontField $ f r)

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
run = withArgs (split " " "-a -s .afp -d M..T -i ln-1.afp -o fixed-1.afp") main

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
    , _InfoCache      :: !(HashTable String FontInfo)
    , _IncrCache      :: !(HashTable String N2)
    , _Opts           :: !Opts
    }

type Config = Int

cacheKey :: String -> NChar -> String
cacheKey font (hi, lo) = (chr' hi : chr' lo : font)
    where chr' = chr . fromEnum

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
    idf <- newIOArray (0x00, 0xFF) []
    ifc <- hashNew (==) hashString
    inc <- hashNew (==) hashString
    opt <- getOpts
    return $ Vars xp x y xo yo im bi fid udc idf ifc inc opt

getOpts = do
    args <- getArgs
    (optsIO, rest, errs) <- return . getOpt Permute options
                                $ if (null args) then ["-h"] else args
    let opts    = foldl (flip ($)) defaultOpts optsIO
        paths   = fontlibPaths opts
        suffix  = fontlibSuffix opts
    showHelp opts
    unless (null rest) $ do
        warn $ "unrecognized arguments: `" ++ joinList " " rest ++ "'\n"
    paths <- filterM checkPath $ fontlibPaths opts
    when (null paths) $ do
        die $ "cannot find a valid font library path"
    fontIsDBCS opts `seq` forM_ errs warn
    return opts { readFontlibAFP = reader paths (fontlibSuffix opts) }
    where
    checkPath path = do
        exists <- doesDirectoryExist path
        unless (exists) $ do
            warn $ "directory does not exist: `" ++ path ++ "'"
        return exists
    reader paths suffix name = do
        (afp:_) <- filterM doesFileExist
                         $ map (++ "/" ++ name ++ suffix) paths
        readAFP afp

infixl 4 &:
l &: v = do
    vars <- ask
    return $ l (_Opts vars) v

infixl 4 &<<
l &<< v = do
    vars <- ask
    liftIO $ l (_Opts vars) v

liftOpt l = do
    vars <- ask
    liftIO $ l (_Opts vars)

defaultOpts = Opts
    { adjustY           = id
    , fontIsDBCS        = requiredOpt usage "dbcs-pattern"
    , dbcsPattern       = Nothing
    , readInputAFP      = requiredOpt usage "input"
    , openOutputAFP     = requiredOpt usage "output"
    , readFontlibAFP    = error "fontlib" -- calculated with the two fields below
    , fontlibPaths      = ["fontlib"]
    , fontlibSuffix     = ""
    , cstrlenCheckUDC   = checkUDC
    , trnToSegments     = segment
    , verbose           = False
    , showHelp          = return ()
    }
    where
    isUDC :: N1 -> Bool
    isUDC hi = hi >= 0x92 && hi <= 0xFE
    -- isDBCS hi = hi >= 0x40 && hi <= 0x91
    checkUDC (cstr, len) = forM_ [0, 2..len-1] $ \off -> do
        hi <- peekByteOff cstr off
        when (isUDC hi) $ fail "UDC"
    segment :: FontField -> [N1] -> VarsIO [N1]
    segment = dbcsHandler
{-
    segment buf = do
        let (pstr, len) = bufToPStrLen buf
        withForeignPtr (castForeignPtr pstr) $ \cstr ->
            return . snd =<< foldM (\off -> do
                hi <- peekByteOff cstr off
                if (isUDC hi) then do
                        lo <- peekByteOff cstr $ off+1
                        return $ SegmentUDC (hi, lo) (0x40, 0x40)
                      else
                        return $ SegmentDBCS (0x4C, 0x41))
                [] [0, 2..len-1]
-}

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
        (\s o -> o { fontIsDBCS     = setMatchDBCS s
                   , dbcsPattern    = Just s })
    , reqArg "f" ["fontlib-path"]   "PATH,PATH..."  "Paths to font libraries"
        (\s o -> o { fontlibPaths   = split "," s })
    , reqArg "i" ["input"]          "FILE"          "Input AFP file"
        (\s o -> o { readInputAFP   = readAFP s })
    , reqArg "o" ["output"]         "FILE"          "Output AFP file"
        (\s o -> o { openOutputAFP  = openBinaryFile s WriteMode })
    , reqArg "s" ["fontlib-suffix"] ".SUFFIX"       "Font library file suffix"
        (\s o -> o { fontlibSuffix  = s })
    , noArg  "v" ["verbose"]                        "Print progress information"
        (\o   -> o { verbose        = True })
    , noArg  "h" ["help"]                           "Show help"
        (\o   -> o { showHelp       = usage "" })
    ]
    where
    setMatchDBCS "M..T" (_:_:'M':_:_:'T':_) = True
    setMatchDBCS "M..T" _                   = False
    setMatchDBCS "NS.." (_:_:'N':'S':_:_:_) = True
    setMatchDBCS "NS.." _                   = False
    setMatchDBCS "NS"   (_:_:'N':'S':_)     = True
    setMatchDBCS "NS"   _                   = False
    setMatchDBCS s a   = (a =~ s)
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
type FontField = String
fromFontField = fromA8

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
    { fontIsDBCS        :: FontField -> Bool
    , dbcsPattern       :: Maybe String
    , adjustY           :: I2 -> I2
    , readInputAFP      :: IO [AFP_]
    , openOutputAFP     :: IO Handle
    , readFontlibAFP    :: String -> IO [AFP_]
    , fontlibPaths      :: [FilePath]
    , fontlibSuffix     :: String
    , cstrlenCheckUDC   :: CStringLen -> IO ()
    , trnToSegments     :: FontField -> [N1] -> VarsIO [N1]
    , verbose           :: Bool
    , showHelp          :: IO ()
    } deriving (Typeable)
