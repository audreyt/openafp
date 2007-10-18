{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module re-exports all Records, Types, Internals and Prelude modules,
-- and provides some toplevel convenience functions.
--
-----------------------------------------------------------------------------

module OpenAFP (
    module OpenAFP.Types,
    module OpenAFP.Records,
    module OpenAFP.Internals,
    module OpenAFP.Prelude.Exts,
    module OpenAFP.Prelude.Utils,
    module OpenAFP.Prelude.Lookups,
    module OpenAFP.Prelude.Instances,
    module OpenAFP.Internals.UConv,

    readAFP, writeAFP, filterAFP,
    readArgs, afp_Chunks,
) where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals
import OpenAFP.Internals.UConv
import OpenAFP.Prelude.Utils
import OpenAFP.Prelude.Lookups
import OpenAFP.Prelude.Instances
import OpenAFP.Prelude.Exts

afp_Chunks :: FilePath -> [AFP_]
afp_Chunks filename = unsafePerformIO $ readAFP filename

writeAFP :: (Binary a) => FilePath -> [a] -> IO ()
writeAFP "-" c = do
    hSetBinaryMode stdout True
    openBinIO_ stdout >>= (`put` c)
writeAFP filename c = do
    fh  <- openBinaryFile filename WriteMode
    openBinIO_ fh >>= (`put` c)
    hClose fh

filterAFP :: FilePath -> FilePath -> [(ChunkType, AFP_ -> IO [AFP_])] -> IO ()
filterAFP input output filters = do
    ifh  <- openBinaryFile input ReadMode
    ibh  <- openBinIO_ ifh
    ofh  <- openBinaryFile output WriteMode
    obh  <- openBinIO_ ofh
    filterAFPChunks ibh obh filters `catchError` \e -> return ()
    hClose ifh
    hClose ofh
    return ()

filterAFPChunks ibh obh filters = do
    eof <- isEOFBin ibh
    if eof then return () else do
	c <- get ibh :: IO AFP_
	filterChunk c filters obh
	filterAFPChunks ibh obh filters

filterChunk c possibleFilters bh
    | null filters  = put bh c
    | otherwise     = mapM_ (\(_, f) -> do
	c' <- f c
	put bh c') filters
    where
	filters = filter (\(t, _) -> (t == chunkType c)) possibleFilters

instance RecChunk FilePath AFP_ N3 Buffer2 where
    readChunks = afp_Chunks

instance Rec Char

readAFP :: (MonadIO m) => FilePath -> m [AFP_]
readAFP "-" = io $ do
    hSetBinaryMode stdin True
    openBinIO_ stdin >>= get

readAFP filename = io $ do
    fh  <- openBinaryFile filename ReadMode
    openBinIO_ fh >>= get

readArgs :: (MonadIO m) => Int -> m [String]
readArgs n = io $ do
    args <- getArgs
    when ((length args) < n) $ do
        pgm <- getProgName
        putStrLn $ "Usage: " ++ pgm ++ " input.afp output.afp"
        exitFailure
    return args

