{-# OPTIONS -O -fglasgow-exts -funbox-strict-fields -fbang-patterns #-}

module Main where
import OpenAFP
import System.Mem
import System.Directory
import Control.Concurrent

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
	(_:_)	-> return args
	[]	-> getDirectoryContents "."
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
scanUDC file = do -- (`catchError` const (return False)) $ do
    fh	    <- openBinaryFile file ReadMode
--  sz      <- fmap fromEnum $ hFileSize fh
--  pstr    <- mallocForeignPtrBytes sz
--  withForeignPtr pstr $ \cstr -> do
--      len <- hGetBuf fh cstr sz
--      bh  <- openBinBuf (pstr, len)
    bh  <- openBinIO_ fh
    do
        rv	<- (`catchError` hdl) $ do
            cs  <- get bh :: IO [AFP_]
            let ptxs = length cs `seq` filter (~~ _PTX) cs
            (`mapM_` ptxs) $ \ptx -> do
                r <- chunkToRecord ptx
                scanPTX (fromRecord r)
            return False
        closeBin bh
        hClose fh
        return rv
    where
    tryOpen = openBinaryFile file ReadMode `catchError` tryErr
    tryErr e
	| isFullError e = do
            threadDelay 200
            tryOpen
	| otherwise = throwError e
    hdl err | Just e <- cast err, isUserError e = return True
	    | otherwise = return False

scanPTX :: PTX -> IO ()
scanPTX ptx = mapM_ ptxGroupScan . splitRecords _PTX_SCFL $ readChunks ptx

ptxGroupScan :: [PTX_] -> IO ()
ptxGroupScan (!scfl:cs) = seq (length cs) $ do
    scflId <- ptx_scfl `applyToChunk` scfl
    case scflId of
	1   -> return ()
	_   -> do
            let trns = filter (~~ _PTX_TRN) cs
            (`mapM_` trns) $ \trn -> do
                r <- chunkToRecord trn
                scanTRN (fromRecord r)

scanTRN :: PTX_TRN -> IO ()
scanTRN trn = do
    withForeignPtr (castForeignPtr pstr) $ \cstr -> do
	forM_ offsets $ \off -> do
	    hi <- peekByteOff cstr off
	    when (isUDC hi) $ do
		throwError (strMsg "Found UDC")
    where
    nstr = ptx_trn trn
    (pstr, len) = bufToPStrLen nstr
    offsets = [0, 2..len-1]
