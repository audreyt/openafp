{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams, ExistentialQuantification, PatternGuards #-}

module Main where
import OpenAFP
import Control.Applicative
import System.Exit
import System.Environment
import Text.LineToPDF.Internals hiding (Encoding)
-- import System.Posix.Resource
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Char8 as C
import qualified Text.LineToPDF.Internals as PDF

deriving instance Eq Encoding
deriving instance Ord Encoding
deriving instance Ix Encoding
deriving instance Bounded Encoding

__1GB__ :: Integer
__1GB__ = 1024 * 1024 * 1024

-- mkLimit :: Integer -> ResourceLimits
-- mkLimit x = ResourceLimits (ResourceLimit x) (ResourceLimit x)

main :: IO ()
main = do
    -- setResourceLimit ResourceTotalMemory (mkLimit __1GB__)
    -- setResourceLimit ResourceCoreFileSize (mkLimit 0)

    hSetBinaryMode stdout True
    args    <- getArgs
    when (null args) $ do
        putStrLn "Usage: afp2line2pdf input.afp > output.pdf"
    forM_ args $ \f -> do
        ft <- guessFileType f
        processFile ft f

data FileType = F_ASCII | F_EBCDIC | F_AFP | F_PDF | F_Unknown deriving Show

-- | Read the first byte to determine its file type:
-- 
--  * '1'    indicates ASCII Plain Text line data
-- 
--  * '\xF1' indicates EBCDIC line data
-- 
--  * 'Z'    indicates AFP file
-- 
--  * '%'    indicates PDF file
-- 
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

imageChunkTypes :: [ChunkType]
imageChunkTypes =
    [ chunkTypeOf _BII
    , chunkTypeOf _BIM
    , chunkTypeOf _MIO
    , chunkTypeOf _IRD
    , chunkTypeOf _IPD
    ]

_blanks :: [C.ByteString]
_blanks = map C.pack
    [ "BLANK"
    , "BLN"
    , "blank"
    , "bln"
    ]

isBlank :: [AFP_] -> Bool
isBlank cs = case find (~~ _BR) cs of
    Just c -> let str = S.drop 2 (packAStr $ br (decodeChunk c)) in
        any (`S.isPrefixOf` str) _blanks
    _ -> False

processFile :: FileType -> FilePath -> IO ()
processFile F_ASCII _ = do
    -- ...Look at each line's first byte to determine what to output...
    return ()
processFile F_AFP f = do
    cs  <- readAFP f

    env <- getEnvironment
    case lookup "AFP2PDF2LINE_SKIP_IMAGE" env of
        Just "1" | not (isBlank cs), any ((`elem` imageChunkTypes) . chunkType) cs -> do
            -- We can't process image data, sorry.
            exitWith (ExitFailure 1)
        _  -> return ()

    hSetBinaryMode stdout True
    pr$ "%PDF-1.2\n" ++ "%\xE2\xE3\xCF\xD3\n"
    (info, root, tPages, resources) <- writeHeader [Latin, Big5, ShiftJIS] 

    forM_ (filter (~~ _PGD) cs) $ \c -> do
        let pgd   = decodeChunk c
            xSize = toPt $ pgd_XPageSize pgd
            ySize = toPt $ pgd_YPageSize pgd
        modifyIORef __MAX_WIDTH__ (max xSize)
        modifyIORef __MAX_HEIGHT__ (max ySize)

    modifyIORef __MAX_WIDTH__ (+30)
    modifyIORef __MAX_HEIGHT__ (+30)

    forM_ (tail $ splitRecords _BPG cs) $ \pageChunks -> do
        -- Start page
        markObj $ \obj -> do
            modifyRef __PAGE__ (obj:)
            pr$ "/Type/Page"
            pr$ "/Parent " ++ show tPages ++ " 0 R"
            pr$ "/Resources " ++ show resources ++ " 0 R"
            pr$ "/Contents " ++ show (succ obj) ++ " 0 R"
            pr$ "/Rotate 0"

        obj <- incrObj
        markLocation obj
        pr$ show obj ++ " 0 obj" ++ "<<"
        pr$ "/Length " ++ show (succ obj) ++ " 0 R"
        pr$ ">>" ++ "stream\n"

        streamStart <- currentLocation
        pr$ "BT\n";

        pageChunks ..>
            [ _PTX  ... ptxDump
            , _MCF  ... mcfHandler
            , _MCF1 ... mcf1Handler
            ]

        -- End Page
        pr$ "ET\n"
        streamEnd <- currentLocation
        pr$ "endstream\n" ++ "endobj\n"

        obj' <- incrObj
        markLocation obj'

        pr$ show obj' ++ " 0 obj\n"
         ++ show (streamEnd - streamStart) ++ "\n"
         ++ "endobj\n"

    pageObjs <-reverse <$> readRef __PAGE__
    markLocation root
    writeObj root $ do
        pr$ "/Type/Catalog" ++ "/Pages "
        pr$ show tPages ++ " 0 R" 

    maxPageWidth <- readIORef __MAX_WIDTH__
    maxPageHeight <- readIORef __MAX_HEIGHT__

    markLocation tPages
    writeObj tPages $ do
        pr$ "/Type/Pages" ++ "/Count "
        pr$ show (length pageObjs)
         ++ "/MediaBox[0 0 "
         ++ show maxPageWidth ++ " " ++ show maxPageHeight
        pr$ "]" ++ "/Kids["
        pr$ concatMap ((++ " 0 R ") . show) pageObjs
        pr$ "]"

    xfer <- currentLocation

    objCount <- incrObj
    pr$ "xref\n" ++ "0 " ++ show objCount ++ "\n"
     ++ "0000000000 65535 f \r"

    writeLocations

    pr$ "trailer\n" ++ "<<" ++ "/Size "
    pr$ show objCount

    pr$ "/Root " ++ show root ++ " 0 R"
    pr$ "/Info " ++ show info ++ " 0 R"
    pr$ ">>\n" ++ "startxref\n"
    pr$ show xfer ++ "\n" ++ "%%EOF\n"

processFile t f = warn $ "Unknown file type: " ++ show t ++ " (" ++ f ++ ")"

{-# NOINLINE __MAX_WIDTH__ #-}
__MAX_WIDTH__ :: IORef Float
__MAX_WIDTH__ = unsafePerformIO $ newIORef 0

{-# NOINLINE __MAX_HEIGHT__ #-}
__MAX_HEIGHT__ :: IORef Float
__MAX_HEIGHT__ = unsafePerformIO $ newIORef 0

{-# NOINLINE _CurrentLine #-}
_CurrentLine :: IORef Float
_CurrentLine = unsafePerformIO $ newIORef 0

{-# NOINLINE _CurrentColumn #-}
_CurrentColumn :: IORef Float
_CurrentColumn = unsafePerformIO $ newIORef 0

{-# NOINLINE _MaxColumn #-}
_MaxColumn :: IORef Int
_MaxColumn = unsafePerformIO $ newIORef 0

{-# NOINLINE _MaxLine #-}
_MaxLine :: IORef Int
_MaxLine = unsafePerformIO $ newIORef 0

{-# NOINLINE _MaxEncoding #-}
_MaxEncoding :: IORef Encoding
_MaxEncoding = unsafePerformIO $ newIORef CP37

{-# NOINLINE _MinFontSize #-}
_MinFontSize :: IORef Size
_MinFontSize = unsafePerformIO $ newIORef 12

lookupFontEncoding :: N1 -> IO (Maybe Encoding)
lookupFontEncoding = hashLookup _FontToEncoding

insertFonts :: [(N1, ByteString)] -> IO ()
insertFonts = mapM_ $ \(i, f) -> do
    let (enc, sz) = fontInfoOf f
    modifyIORef _MinFontSize $ \szMin -> case szMin of
        0   -> sz
        _   -> min szMin sz
    modifyIORef _MaxEncoding (max enc)
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

convertEncoding :: Encoding -> PDF.Encoding
convertEncoding CP37  = Latin
convertEncoding CP835 = Big5
convertEncoding CP939 = ShiftJIS
convertEncoding CP950 = Big5

ptxGroupDump :: [PTX_] -> IO ()
ptxGroupDump (scfl:cs) | scfl ~~ _PTX_SCFL = do
    let scflId = ptx_scfl (decodeChunk scfl)

    rv <- lookupFontEncoding scflId
    case rv of
        Nothing -> return ()
        Just curEncoding -> do
            height <- readIORef __MAX_HEIGHT__
            curSize <- readIORef _MinFontSize
            let display (enc, getBStr) = do
                    font <- case enc of
                        Latin   -> do
                            (fontOf . convertEncoding) <$> readIORef _MaxEncoding
                        _       -> return (fontOf enc)
                    pr$ "/F" ++ font ++ " " ++ show curSize ++ " Tf\n"
                    pr$ show curSize ++ " TL\n"
                    col <- readIORef _CurrentColumn
                    ln  <- readIORef _CurrentLine
                    pr$ "1 0 0 1 " ++ show (col + 25) ++ " " ++ show (height - ln - 25) ++ " Tm("
                    let escape ch = C.intercalate (C.pack ['\\', ch]) . C.split ch
                        escapeTxt = escape '(' . escape ')' . escape '\\'
                    origBStr <- getBStr
                    let origLen     = S.length origBStr
                        escapedLen  = S.length escapedBStr
                        escapedBStr = escapeTxt origBStr
                    C.putStr escapedBStr
                    modifyRef __POS__ (+ escapedLen)
                    pr$ ")Tj\n"
                    modifyIORef _CurrentColumn (+ (realToFrac (origLen * curSize) * 0.5))
            cs ..>
                [ _PTX_TRN ... \trn -> display $ case curEncoding of
                    CP37    -> (Latin, return $ packAStr' (ptx_trn trn))
                    CP835   -> (Big5, pack835 (ptx_trn trn))
                    CP939   -> (ShiftJIS, pack939 (ptx_trn trn))
                    CP950   -> (Big5, return $ packBuf (ptx_trn trn))
                , _PTX_BLN ... \_ -> do
                    writeIORef _CurrentColumn 0 -- XXX
                    modifyIORef _CurrentLine (+ realToFrac curSize)
                , _PTX_AMB ... \x -> do
                    -- hPrint stderr ("AMB", fromEnum $ ptx_amb x)
                    movePosition Absolute _CurrentLine . ptx_amb $ x
                , _PTX_RMB ... \x -> do
                    -- hPrint stderr ("RMB", fromEnum $ ptx_rmb x)
                    movePosition Relative _CurrentLine . ptx_rmb $ x
                , _PTX_AMI ... movePosition Absolute _CurrentColumn . ptx_ami
                , _PTX_RMI ... movePosition Relative _CurrentColumn . ptx_rmi
                ]
ptxGroupDump _ = return ()

data Position = Absolute | Relative 

movePosition :: Position -> IORef Float -> N2 -> IO ()
movePosition p ref n = do
    case p of
        Absolute -> writeIORef ref (toPt n)
        Relative -> modifyIORef ref (+ (toPt n))


toPt :: Enum a => a -> Float
toPt x = realToFrac (fromEnum x) * 0.3

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

-- Target size = AFP size / 3
