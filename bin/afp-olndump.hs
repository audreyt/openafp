import OpenAFP
import Control.Applicative
import Data.Binary.Get
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

data F = MkF
    { _OFLLen       :: N4
    , _Checksum     :: N4
    , _OIDLen       :: N2
    , _OIDName      :: ByteString
    , _ODescLen     :: N2
    , _ObjDesc      :: Desc
    , _ObjData      :: ByteString
    }
    deriving Show

data Desc
    = CMap
        { _ObjType          :: N1
        , _Precedence       :: N1
        , _Linkage          :: N1
        , _WritingDirection :: N1
        , _GCSGID           :: N2
        , _CPGID            :: N2
        }
    | CID
        { _ObjType          :: N1
        , _Precedence       :: N1
        , _MaximumV         :: N2
        , _MaximumW         :: N2
        }
    | NA        { _ObjType  :: N1 }
    | PFB       { _ObjType  :: N1 }
    | AFM       { _ObjType  :: N1 }
    | FMap      { _ObjType  :: N1 }
    | Reserved  { _ObjType  :: N1 }
    deriving Show

descExt :: Desc -> String
descExt CMap{}      = "CMP"
descExt CID{}       = "CID"
descExt NA{}        = "NA"
descExt PFB{}       = "PFB"
descExt AFM{}       = "AFM"
descExt FMap{}      = "DAT"
descExt Reserved{}  = "RES"

instance Binary Desc where
    put = undefined
    get = do
        objType <- get
        case objType of
            0 -> return $ NA objType
            1 -> liftM5 (CMap objType) get get get get get
            5 -> liftM3 (CID objType) get get get
            6 -> return $ PFB objType
            7 -> return $ AFM objType
            8 -> return $ FMap objType
            _ -> return $ Reserved objType

instance Binary F where
    put = undefined
    get = do
        oflLen      <- get
        checksum    <- get
        oidLen      <- get
        let szOID = fromEnum oidLen - 2
        oidName     <- getByteString szOID
        oDescLen    <- get
        let szDesc = fromEnum oDescLen - 2
        objDesc     <- decodeBuf <$> getByteString szDesc
        objData     <- getByteString (fromEnum oflLen - 12 - szOID - szDesc)
        return $ MkF oflLen checksum oidLen oidName oDescLen objDesc objData
        where
        decodeBuf :: Binary a => ByteString -> a
        decodeBuf = decode . L.fromChunks . (:[])

main :: IO ()
main = do
    args <- getArgs
    if null args then error "Usage: afp-olndump input.oln [outputDirectory]" else do
    let inputFile   = head args
        outDir      = case args of
            (_:d:_) -> d
            _       -> let d = takeWhile (/= '.') inputFile in
                if d == inputFile then d ++ ".out" else d
    cs <- filter (~~ _FNG) <$> readAFP (head args)
    let fs = decodeList $ L.fromChunks (map (packBuf . fng . decodeChunk) cs)
    createDirectoryIfMissing True outDir
    mapM_ (dump outDir) fs

dump :: FilePath -> F -> IO ()
dump d f = do
    putStrLn $ unwords [ file, ":", show len, "bytes" ]
    C.writeFile file dat
    where
    ext  = descExt $ _ObjDesc f
    file = d ++ "/" ++ C.unpack (_OIDName f) ++ ('.':ext)
    len  = C.length dat
    dat  = _ObjData f
