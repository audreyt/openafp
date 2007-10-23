{-# OPTIONS -O2 -fglasgow-exts #-}

module Main where
import OpenAFP
import CP835
import qualified Data.IntMap as IM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    hSetBinaryMode stdout True
    (getArgs >>=) . mapM_ $ \f -> do
        cs  <- readAFP f
        forM_ (splitRecords _PGD cs) $ \page -> do
            page ..>
                [ _MCF  ... mcfHandler
                , _MCF1 ... mcf1Handler
                , _PTX  ... ptxDump
                ]
            dumpPageContent


dumpPageContent :: IO ()
dumpPageContent = do
    pg  <- readIORef _CurrentPage
    writeIORef _CurrentPage IM.empty
    forM_ (IM.elems pg) $ \line -> do
        writeIORef _CurrentColumn 0
        forM_ (IM.toAscList line) $ \(col, str) -> do
            cur <- readIORef _CurrentColumn
            B.putStr (B.take (col - cur) _Spaces)
            B.putStr str
            writeIORef _CurrentColumn (col + B.length str)
        B.putStr _NewLine
    unless (IM.null pg) $ B.putStr _NewPage

_Spaces  = B.replicate 65536 0x20
_NewLine = C.pack "\r\n"
_NewPage = C.pack "\r\n\x0C\r\n"

data Encoding = CP37 | CP835
    deriving Show

type Size = Int

fontInfoOf :: String -> (Encoding, Size)
fontInfoOf f
    | ('T':_) <- dropWhile isSpace (reverse f)
    = (CP835, sz `div` 2)
    | otherwise
    = (CP37, sz)
    where
    sz = read . reverse . takeWhile isDigit . dropWhile (not . isDigit) . reverse $ f

{-# NOINLINE _FontToEncoding #-}
_FontToEncoding :: HashTable N1 Encoding
_FontToEncoding = unsafePerformIO $ hashNew (==) fromIntegral

{-# NOINLINE _MinFontSize #-}
_MinFontSize :: IORef Int
_MinFontSize = unsafePerformIO $ newIORef 0

{-# NOINLINE _CurrentPage #-}
_CurrentPage :: IORef Page
_CurrentPage = unsafePerformIO $ newIORef IM.empty

{-# NOINLINE _CurrentLine #-}
_CurrentLine :: IORef Int
_CurrentLine = unsafePerformIO $ newIORef 0

{-# NOINLINE _CurrentColumn #-}
_CurrentColumn :: IORef Int
_CurrentColumn = unsafePerformIO $ newIORef 0

-- | Record font Id to Name mappings in MCF's RLI and FQN chunks.
mcfHandler :: MCF -> IO ()
mcfHandler r = do
    readChunks r ..>
        [ _MCF_T ... \mcf -> do
            let cs = readChunks mcf
            ids   <- sequence [ t_rli `applyToChunk` c | c <- cs, c ~~ _T_RLI ]
            fonts <- sequence [ t_fqn `applyToChunk` c | c <- cs, c ~~ _T_FQN ]
            insertFonts (ids `zip` map fromA8 fonts)
        ]


insertFonts :: [(N1, String)] -> IO ()
insertFonts = mapM_ $ \(i, f) -> do
    let (enc, sz) = fontInfoOf f
    modifyIORef _MinFontSize $ \szMin -> case szMin of
        0   -> sz
        _   -> min szMin sz
    hashInsert _FontToEncoding i enc

-- | Record font Id to Name mappings in MCF1's Data chunks.
mcf1Handler :: MCF1 -> IO ()
mcf1Handler r = do
    insertFonts
        [ (mcf1_CodedFontLocalId mcf1, fromA8 $ mcf1_CodedFontName mcf1)
            | Record mcf1 <- readData r
        ]

ptxDump :: PTX -> IO ()
ptxDump ptx = mapM_ ptxGroupDump .
    splitRecords _PTX_SCFL $ readChunks ptx

-- A Page is a IntMap from line-number to a map from column-number to bytestring.
type Page = IM.IntMap Line
type Line = IM.IntMap B.ByteString

insertText :: B.ByteString -> IO ()
insertText str = do
    ln  <- readIORef _CurrentLine
    col <- readIORef _CurrentColumn
    modifyIORef _CurrentPage $ \pg -> case IM.lookup ln pg of
        Nothing     -> IM.insert ln (IM.singleton col str) pg
        Just    im  -> IM.insert ln (IM.insert col str im) pg

ptxGroupDump :: [PTX_] -> IO ()
ptxGroupDump (scfl:cs) = do
    scflId  <- ptx_scfl `applyToChunk` scfl
    rv      <- hashLookup _FontToEncoding scflId
    case rv of
        Nothing          -> return ()
        Just curEncoding -> do
            cs ..> 
                [ _PTX_TRN ... \trn -> case curEncoding of
                    CP37    -> packAStr (ptx_trn trn) >>= \bstr -> do
                        insertText bstr
                        modifyIORef _CurrentColumn (+ B.length bstr)
                    CP835   -> pack835 (ptx_trn trn) >>= \bstr -> do
                        insertText bstr
                        modifyIORef _CurrentColumn (+ B.length bstr)
                , _PTX_BLN ... \_ -> do
                    writeIORef _CurrentColumn 0
                    modifyIORef _CurrentLine (+1)
                , _PTX_AMB ... \x -> do
                    minSize <- readIORef _MinFontSize
                    writeIORef _CurrentLine (fromEnum (ptx_amb x) `div` minSize)
                , _PTX_AMI ... \x -> do
                    minSize <- readIORef _MinFontSize
                    let offset  = (fromEnum $ ptx_ami x)
                        pos'    = offset `div` minSize
                    writeIORef _CurrentColumn pos'
                ]

packAStr :: AStr -> IO B.ByteString
packAStr astr = do
    let (pstr, len) = bufToPStrLen astr 
    withForeignPtr (castForeignPtr pstr) $ \cstr -> do
        fmap (B.map (ebc2ascWord !)) (B.packCStringLen (cstr, len))

pack835 :: NStr -> IO B.ByteString
pack835 nstr = do
    let (pstr, len) = bufToPStrLen nstr 
    withForeignPtr (castForeignPtr pstr) $ \cstr -> do
        forM_ [0..(len `div` 2)-1] $ \i -> do
            hi  <- peekElemOff cstr (i*2)   :: IO Word8
            lo  <- peekElemOff cstr (i*2+1) :: IO Word8
            let cp950       = convert835to950 (fromEnum hi * 256 + fromEnum lo)
                (hi', lo')  = cp950 `divMod` 256
            pokeElemOff cstr (i*2)   (toEnum hi')
            pokeElemOff cstr (i*2+1) (toEnum lo')
        B.packCStringLen (castPtr cstr, len)

ebc2ascWord :: UArray Word8 Word8
ebc2ascWord = listArray (0x00, 0xff) [
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
