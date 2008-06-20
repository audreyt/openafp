{-# OPTIONS_GHC -O2 -fglasgow-exts #-}

module Main where
import OpenAFP
import Data.Monoid
import System.Posix.Resource
import qualified Data.IntMap as IM
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Char8 as C

__1GB__ :: Integer
__1GB__ = 1024 * 1024 * 1024

mkLimit :: Integer -> ResourceLimits
mkLimit x = ResourceLimits (ResourceLimit x) (ResourceLimit x)

main :: IO ()
main = do
    setResourceLimit ResourceTotalMemory (mkLimit __1GB__)
    setResourceLimit ResourceCoreFileSize (mkLimit 0)

    hSetBinaryMode stdout True
    args    <- getArgs
    when (null args) $ do
        putStrLn "Usage: afp2line input.afp ... > output.txt"
    forM_ args $ \f -> do
        ft <- guessFileType f
        processFile ft f

data FileType = F_ASCII | F_EBCDIC | F_AFP | F_PDF | F_Unknown deriving Show

guessFileType :: FilePath -> IO FileType
guessFileType fn = do
    fh <- openBinaryFile fn ReadMode
    bs <- S.hGet fh 1
    hClose fh
    return $ if S.null bs then F_Unknown else case C.head bs of
        'Z'     -> F_AFP
        '%'     -> F_PDF
        '1'     -> F_ASCII
        '0'     -> F_ASCII
        ' '     -> F_ASCII
        '\xF0'  -> F_EBCDIC
        '\xF1'  -> F_EBCDIC
        '@'     -> F_EBCDIC
        _       -> F_Unknown

processFile :: FileType -> FilePath -> IO ()
processFile F_ASCII f = do
    -- ...Look at each line's first byte to determine what to output...
    return ()
processFile F_AFP f = do
    -- Read the first byte to determine its file type:
    -- '1'    indicates ASCII Plain Text line data
    -- '\xF1' indicates EBCDIC line data
    -- 'Z'    indicates AFP file
    -- '%'    indicates PDF file
    cs  <- readAFP f
    forM_ (splitRecords _PGD cs) $ \page -> do
        page ..>
            [ _PTX  ... ptxDump
            , _MCF  ... mcfHandler
            , _MCF1 ... mcf1Handler
            ]
        dumpPageContent
processFile t f = warn $ "Unknown file type: " ++ show t ++ " (" ++ f ++ ")"

dumpPageContent :: IO ()
dumpPageContent = do
    MkPage pg  <- readIORef _CurrentPage
    writeIORef _CurrentPage mempty
    if IM.null pg then return () else do
        forM_ (IM.elems pg) $ \(MkLine line) -> do
            writeIORef _CurrentColumn 0
            forM_ (IM.toAscList line) $ \(col, str) -> do
                cur <- readIORef _CurrentColumn
                S.putStr $ S.take (col - cur) _Spaces
                S.putStr str
                writeIORef _CurrentColumn (col + S.length str)
            S.putStr _NewLine
        S.putStr _NewPage

_Spaces, _NewLine, _NewPage :: ByteString
_Spaces  = S.replicate 4096 0x20
_NewLine = C.pack "\r\n"
_NewPage = C.pack "\r\n\x0C\r\n"

{-# NOINLINE _CurrentPage #-}
_CurrentPage :: IORef Page
_CurrentPage = unsafePerformIO $ newIORef mempty

{-# NOINLINE _CurrentLine #-}
_CurrentLine :: IORef Int
_CurrentLine = unsafePerformIO $ newIORef 0

{-# NOINLINE _CurrentColumn #-}
_CurrentColumn :: IORef Int
_CurrentColumn = unsafePerformIO $ newIORef 0

{-# NOINLINE _MinFontSize #-}
_MinFontSize :: IORef Size
_MinFontSize = unsafePerformIO $ newIORef 0

lookupFontEncoding :: N1 -> IO (Maybe Encoding)
lookupFontEncoding = hashLookup _FontToEncoding

insertFonts :: [(N1, ByteString)] -> IO ()
insertFonts = mapM_ $ \(i, f) -> do
    let (enc, sz) = fontInfoOf f
    modifyIORef _MinFontSize $ \szMin -> case szMin of
        0   -> sz
        _   -> min szMin sz
    hashInsert _FontToEncoding i enc

{-# NOINLINE _FontToEncoding #-}
_FontToEncoding :: HashTable N1 Encoding
_FontToEncoding = unsafePerformIO $ hashNew (==) fromIntegral

-- | Record font Id to Name mappings in MCF's RLI and FQN chunks.
mcfHandler :: MCF -> IO ()
mcfHandler r = do
    readChunks r ..>
        [ _MCF_T ... \mcf -> do
            let cs    = readChunks mcf
                ids   = [ t_rli (decodeChunk c) | c <- cs, c ~~ _T_RLI ]
                fonts = [ t_fqn (decodeChunk c) | c <- cs, c ~~ _T_FQN ]
            insertFonts (ids `zip` map packAStr fonts)
        ]

-- | Record font Id to Name mappings in MCF1's Data chunks.
mcf1Handler :: MCF1 -> IO ()
mcf1Handler r = do
    insertFonts
        [ (mcf1_CodedFontLocalId mcf1, packA8 $ mcf1_CodedFontName mcf1)
            | Record mcf1 <- readData r
        ]

ptxDump :: PTX -> IO ()
ptxDump ptx = mapM_ ptxGroupDump . splitRecords _PTX_SCFL $ readChunks ptx

-- A Page is a IntMap from line-number to a map from column-number to bytestring.
newtype Page = MkPage { fromPage :: IM.IntMap Line } deriving (Show, Monoid)
newtype Line = MkLine { lineStrs :: IM.IntMap S.ByteString } deriving Show

insertText :: S.ByteString -> IO ()
insertText str = do
    ln  <- readIORef _CurrentLine
    col <- readIORef _CurrentColumn
    modifyIORef _CurrentPage $ \(MkPage pg) -> MkPage $! case IM.lookup ln pg of
        Nothing          -> IM.insert ln (MkLine (IM.singleton col str)) pg
        Just (MkLine im) -> IM.insert ln (MkLine (IM.insert col str im)) pg

ptxGroupDump :: [PTX_] -> IO ()
ptxGroupDump (scfl:cs) = do
    let scflId = ptx_scfl (decodeChunk scfl)
    curEncoding <- lookupFontEncoding scflId
    cs ..>
        [ _PTX_TRN ... \trn -> case curEncoding of
            Just CP37    -> let bstr = packAStr' (ptx_trn trn) in do
                insertText bstr
                modifyIORef _CurrentColumn (+ S.length bstr)
            Just CP835   -> pack835 (ptx_trn trn) >>= \bstr -> do
                insertText bstr
                modifyIORef _CurrentColumn (+ S.length bstr)
            Just CP939   -> pack939 (ptx_trn trn) >>= \bstr -> do
                insertText bstr
                modifyIORef _CurrentColumn (+ S.length bstr)
            Just CP950   -> let bstr = packBuf (ptx_trn trn) in do
                insertText bstr
                modifyIORef _CurrentColumn (+ S.length bstr)
            _            -> fail "TRN without SCFL?"
        , _PTX_BLN ... \_ -> do
            writeIORef _CurrentColumn 0
            modifyIORef _CurrentLine (+1)
        , _PTX_AMB ... movePosition Absolute _CurrentLine   . ptx_amb
        , _PTX_RMB ... movePosition Relative _CurrentLine   . ptx_rmb
        , _PTX_AMI ... movePosition Absolute _CurrentColumn . ptx_ami
        , _PTX_RMI ... movePosition Relative _CurrentColumn . ptx_rmi
        ]

data Position = Absolute | Relative 

movePosition :: Position -> IORef Int -> N2 -> IO ()
movePosition p ref n = do
    minSize <- readIORef _MinFontSize
    let offset = fromEnum n `div` minSize
    case p of
        Absolute -> writeIORef ref offset
        Relative -> modifyIORef ref (+ offset)


packAStr' :: AStr -> S.ByteString
packAStr' astr = S.map (ebc2ascIsPrintW8 !) (packBuf astr)

{-# INLINE pack835 #-}
{-# INLINE pack939 #-}
pack835, pack939 :: NStr -> IO S.ByteString
pack835 = packWith convert835to950
pack939 = packWith convert939to932

{-# INLINE packWith #-}
packWith :: (Int -> Int) -> NStr -> IO S.ByteString
packWith f nstr = S.unsafeUseAsCStringLen (packBuf nstr) $ \(src, len) -> S.create len $ \target -> do
    let s = castPtr src
    let t = castPtr target
    forM_ [0..(len `div` 2)-1] $ \i -> do
        hi  <- peekByteOff s (i*2)       :: IO Word8
        lo  <- peekByteOff s (i*2+1)     :: IO Word8
        let ch         = f (fromEnum hi * 256 + fromEnum lo)
            (hi', lo') = ch `divMod` 256
        pokeByteOff t (i*2)   (toEnum hi' :: Word8)
        pokeByteOff t (i*2+1) (toEnum lo' :: Word8)
