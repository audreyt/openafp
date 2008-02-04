{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP
import System.Exit
import System.FilePath
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

    let (preamble, rest) = break (~~ _BPG) cs
        _epg      = encodeChunk $ Record _EPG
        _eng      = encodeChunk $ Record _ENG
        _edt      = encodeChunk $ Record _EDT
        postamble | any (~~ _BNG) preamble  = [_epg, _eng, _edt]
                  | otherwise               = [_epg, _edt]

    smallOpened <- newIORef Nothing
    largeOpened <- newIORef Nothing

    forM_ ([0..] `zip` splitPages rest) $ \(i, page) -> do
        fh  <- initFh preamble $ case pageSizeOf page of
                PSmall  -> (smallOpened, dir `combine` "small_" ++ fn)
                _       -> (largeOpened, dir `combine` "large_" ++ fn)
        L.hPut fh $ encodeList page

    finalizeFh postamble smallOpened
    finalizeFh postamble largeOpened

-- Find the non-zero AMB with lowest number
pageSizeOf :: (?maxSmallPages :: Int) => [AFP_] -> PageSize
pageSizeOf [] = PSmall
pageSizeOf cs = case sortBy compareAMB rows of
    []      -> PSmall -- A page with no text?
    (row:_) -> case sortBy compareTRN [packAStr $ ptx_trn (decodeChunk c) | c <- row, c ~~ _PTX_TRN] of
        []      -> PSmall -- A column with no TRN?
        (col:_) -> case C.words col of
            []      -> PSmall -- An empty TRN?
            toks    -> case C.readInt (last toks) of
                Nothing -> PSmall   -- A non-numeric token?
                Just (i, _) | i <= ?maxSmallPages -> trace (show col ++ ": Small") PSmall
                            | otherwise           -> trace (show col ++ ": Large") PLarge
    where
    rows = tail . splitRecords _PTX_AMB $ concat [ptx_Chunks $ decodeChunk c | c <- cs, c ~~ _PTX ]
    compareTRN x y = compare (C.length y) (C.length x)
    compareAMB (x:_) (y:_) = compare (ambOf x) (ambOf y)
    compareAMB _ _ = EQ
    ambOf c = case ptx_amb $ decodeChunk c of
        0   -> maxBound -- We don't really care about rows with AMB 0.
        x   -> x

splitPages :: [AFP_] -> [[AFP_]]
splitPages cs = case rest of
    []      -> []
    (_:xs)  -> (this:splitPages xs)
    where
    (this, rest) = break (~~ _EPG) cs
