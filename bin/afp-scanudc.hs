{-# OPTIONS -O -fglasgow-exts -funbox-strict-fields -fbang-patterns #-}

module Main where
import OpenAFP
import System.Mem
import System.Directory
import Control.Concurrent
import qualified Control.Exception as E (try, catch, throwIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

isUDC :: N1 -> Bool
isUDC hi = hi >= 0x92 && hi <= 0xFE

-- | The main program: Scan all files in the argument line
-- (by default, every file in the current directory if none is specified)
-- for UDC characters, then output those that has UDC characters set
-- so the calling program can make use of that list to call afp-udcfix
-- to correct those UDC characters. (N.B.: Maybe we should launch afp-udcfix
-- by ourselves here?)

main :: IO ()
main = do
    args    <- getArgs
    files   <- filterM doesFileExist =<< case args of
        (_:_)   -> return args
        []      -> getDirectoryContents "."
    seq (length files) . (`mapM_` sort files) $ \fn -> do
        rv <- scanUDC fn
        when rv (putStrLn fn)
    -- doAllLists files
    where
    doAllLists [] = return ()
    doAllLists xs = do
        mvs <- doHyperLists these
        rvs <- mapM takeMVar mvs
        mapM_ putStrLn [ file | (True, file) <- rvs `zip` these ]
        performGC
        doAllLists rest
        where
        (these, rest) = splitAt 10 xs
    doHyperLists [] = return []
    doHyperLists (fn:fns) = do
        mv  <- newEmptyMVar
        forkIO $ do
            rv <- scanUDC fn
            putMVar mv rv
        mvs <- doHyperLists fns
        return (mv:mvs)

scanUDC :: FilePath -> IO Bool
scanUDC file = do
    rv <- E.try $ readAFP file
    case rv of
        Right cs    -> (`E.catch` hdl) $ do
            let ptxs = length cs `seq` filter (~~ _PTX) cs
            mapM_ (scanPTX . decodeChunk) ptxs
            return False
        _           -> return False -- skip non-afp files
    where
    tryOpen = openBinaryFile file ReadMode `E.catch` tryErr
    tryErr (IOException ioe) | isFullError ioe = threadDelay 200 >> tryOpen
    tryErr e = E.throwIO e
    hdl (IOException ioe) | Just e <- cast ioe, isUserError e = return True
    hdl _ = return False

scanPTX :: PTX -> IO ()
scanPTX ptx = mapM_ ptxGroupScan . splitRecords _PTX_SCFL $ readChunks ptx

ptxGroupScan :: [PTX_] -> IO ()
ptxGroupScan (!scfl:cs) = seq (length cs) $ do
    scflId <- ptx_scfl `applyToChunk` scfl
    case scflId of
        1   -> return ()
        _   -> do
            let trns = filter (~~ _PTX_TRN) cs
            (`mapM_` trns) $ \trn -> scanTRN (decodeChunk trn)

scanTRN :: PTX_TRN -> IO ()
scanTRN trn = B.unsafeUseAsCStringLen (packBuf $ ptx_trn trn) $ \(cstr, len) -> do
    forM_ [0, 2..len-1] $ \off -> do
        hi <- peekByteOff cstr off
        when (isUDC hi) $ do
            -- lo <- peekByteOff cstr (off+1)
            -- print [hi, lo]
            throwError (strMsg "Found UDC")
