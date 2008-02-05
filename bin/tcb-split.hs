{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP
import System.Exit
import System.FilePath
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

-- Algorithm:
--  Gather everything up to first BPG
--  write out each BPG/EPG chunks
--  append ENG+EDT

data PageSize = PSmall | PLarge deriving (Show)

type MaybeHandleRef = IORef (Maybe Handle)

initFh :: [AFP_] -> (MaybeHandleRef, FilePath) -> IO Handle
initFh chunks (ref, fn) = do
    rv  <- readIORef ref
    case rv of
        Just fh -> return fh
        _       -> do
            fh <- openBinaryFile fn WriteMode
            L.hPut fh $ encodeList chunks
            writeIORef ref (Just fh)
            return fh

finalizeFh :: [AFP_] -> MaybeHandleRef -> IO ()
finalizeFh chunks ref = do
    rv  <- readIORef ref
    case rv of
        Just fh -> do
            L.hPut fh $ encodeList chunks
            hClose fh
        _       -> return ()

main :: IO ()
main = do
    args    <- getArgs

    let (inFile, maxSmallPages) = case args of
            []      -> error "Usage: tcb-split file.afp [max-small-pages (defaults to 3)]"
            [x]     -> (x, 3)
            (x:y:_) -> (x, read y)
        (dir, fn) = splitFileName inFile

    let ?maxSmallPages  = maxSmallPages

    cs      <- readAFP inFile

    let (preamble:rest) = splitPages cs
        _edt            = encodeChunk $ Record _EDT

    smallOpened <- newIORef Nothing
    largeOpened <- newIORef Nothing

    forM_ rest $ \page -> do
        fh  <- initFh preamble $ case pageSizeOf page of
                PSmall  -> (smallOpened, dir `combine` "small_" ++ fn)
                _       -> (largeOpened, dir `combine` "large_" ++ fn)
        L.hPut fh $ encodeList page

    finalizeFh [_edt] smallOpened
    finalizeFh [_edt] largeOpened

isBeginPage :: AFP_ -> Bool
isBeginPage t = (t ~~ _BPG) || (t ~~ _BNG)

-- Find the non-zero AMB with lowest number
pageSizeOf :: (?maxSmallPages :: Int) => [AFP_] -> PageSize
pageSizeOf [] = PSmall
pageSizeOf cs = case sortBy compareAMB rows of
    []      -> PSmall -- A page with no text?
    (row:_) -> case sortBy compareTRN [packNStr $ ptx_trn (decodeChunk c) | c <- row, c ~~ _PTX_TRN] of
        []      -> PSmall -- A column with no TRN?
        (col:_) -> case [ S.map fromDigitLike x | x <- S.splitWith (not . isDigitLike) col, not (S.null x) ] of
            []  -> PSmall -- No digits :-/
            xs  -> case C.readInt (last xs) of
                Nothing -> trace (shows xs ": Non-parsable") PSmall   -- A non-numeric token?
                Just (i, _) | i <= ?maxSmallPages -> trace (shows xs ": Small") PSmall
                            | otherwise           -> trace (shows xs ": Large") PLarge
    where
    rows = case splitRecords _PTX_AMB $ concat [ptx_Chunks $ decodeChunk c | c <- cs, c ~~ _PTX ] of
        []      -> []
        (_:xs)  -> xs
    compareTRN x y = compare (C.length y) (C.length x)
    compareAMB (x:_) (y:_) = compare (ambOf x) (ambOf y)
    compareAMB _ _ = EQ
    ambOf c = case ptx_amb $ decodeChunk c of
        0   -> maxBound -- We don't really care about rows with AMB 0.
        x   -> x

fromDigitLike :: Word8 -> Word8
fromDigitLike n
    | n >= 0xF0 = n - 0xC0
    | otherwise = n

isDigitLike :: Word8 -> Bool
isDigitLike n = (n >= 0x30 && n <= 0x39) || (n >= 0xF0 && n <= 0xF9)

-- | Selects words corresponding to white-space characters in the Latin-1 range
-- ordered by frequency. 
isSpaceWord8' :: Word8 -> Bool
isSpaceWord8' w =
    w == 0x20 ||
    w == 0x00 || -- This Case Is Specific To Us
    w == 0x0A || -- LF, \n
    w == 0x09 || -- HT, \t      
    w == 0x0C || -- FF, \f      
    w == 0x0D || -- CR, \r      
    w == 0x0B || -- VT, \v      
    w == 0xA0    -- spotted by QC..
{-# INLINE isSpaceWord8' #-}


splitPages :: [AFP_] -> [[AFP_]]
splitPages cs = if null rest then [this] else case splitPages rest' of
    []      -> [this, rest]
    (y:ys)  -> (this:(begins ++ y):ys)
    where
    (this, rest)    = break isBeginPage cs
    (begins, rest') = span isBeginPage rest

