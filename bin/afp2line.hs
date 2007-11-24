{-# OPTIONS_GHC -O2 -fglasgow-exts #-}

module Main where
import OpenAFP
import CP835
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
        cs  <- readAFP f
        forM_ (splitRecords _PGD cs) $ \page -> do
            page ..>
                [ _PTX  ... ptxDump
                , _MCF  ... mcfHandler
                , _MCF1 ... mcf1Handler
                ]
            dumpPageContent

dumpPageContent :: IO ()
dumpPageContent = do
    pg  <- readIORef _CurrentPage
    writeIORef _CurrentPage IM.empty
    if IM.null pg then return () else do
        forM_ (IM.elems pg) $ \line -> do
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
_CurrentPage = unsafePerformIO $ newIORef IM.empty

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
type Page = IM.IntMap Line
type Line = IM.IntMap S.ByteString

insertText :: S.ByteString -> IO ()
insertText str = do
    ln  <- readIORef _CurrentLine
    col <- readIORef _CurrentColumn
    modifyIORef _CurrentPage $ \pg -> case IM.lookup ln pg of
        Nothing     -> IM.insert ln (IM.singleton col str) pg
        Just    im  -> IM.insert ln (IM.insert col str im) pg

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
packAStr' astr = S.map (ebc2ascWord !) (packBuf astr)

pack835 :: NStr -> IO S.ByteString
pack835 nstr = S.unsafeUseAsCStringLen (packBuf nstr) $ \(src, len) -> S.create len $ \target -> do
    let s = castPtr src
    let t = castPtr target
    forM_ [0..(len `div` 2)-1] $ \i -> do
        hi  <- peekByteOff s (i*2)       :: IO Word8
        lo  <- peekByteOff s (i*2+1)     :: IO Word8
        let cp950       = convert835to950 (fromEnum hi * 256 + fromEnum lo)
            (hi', lo')  = cp950 `divMod` 256
        pokeByteOff t (i*2)   (toEnum hi' :: Word8)
        pokeByteOff t (i*2+1) (toEnum lo' :: Word8)

ebc2ascWord :: UArray Word8 Word8
ebc2ascWord = listArray (0x00, 0xFF) [
        0x20, 0x20, 0x20, 0x20, 0x20, 0x09, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x0D, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x0A, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x5B, 0x2E, 0x3C, 0x28, 0x2B, 0x21,
        0x26, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x5D, 0x24, 0x2A, 0x29, 0x3B, 0x5E,
        0x2D, 0x2F, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x7C, 0x2C, 0x25, 0x5F, 0x3E, 0x3F,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x60, 0x3A, 0x23, 0x40, 0x27, 0x3D, 0x22,
        0x20, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
        0x68, 0x69, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70,
        0x71, 0x72, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x7E, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
        0x79, 0x7A, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x7B, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
        0x48, 0x49, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x7D, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50,
        0x51, 0x52, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x5C, 0x20, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
        0x59, 0x5A, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20,
        0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
        0x38, 0x39, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20 ]
