{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Prelude.Utils
-- Copyright   :  (c) Autrijus Tang 2004
-- License     :  BSD-style
-- 
-- Maintainer  :  autrijus@autrijus.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module provides various AFP manipulation utilities.
--
-----------------------------------------------------------------------------

module OpenAFP.Prelude.Utils where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

infixl 5 $$
infixl 5 $=
infixl 5 +=
infixl 5 %?
infixl 5 %:
infixl 5 @=
infixl 5 %=
infixl 5 %%=

readVar l = do
    vars <- ask
    liftIO $ readIORef $ l vars

readArray l i = do
    vars <- ask
    liftIO $ readIOArray (l vars) i

l $$ s = do
    vars    <- ask
    when (l vars) $ do
        liftIO . putStrLn $ "*** " ++ s

l $= f = \v -> do
    vars    <- ask
    liftIO $ writeIORef (l vars) (f v)
    return ()
l += f = \v -> do
    vars    <- ask
    v'      <- liftIO $ readIORef (l vars)
    liftIO $ writeIORef (l vars) (f v + v')
    return ()
l @= v = do
    vars    <- ask
    v'      <- liftIO $ readIORef (l vars)
    liftIO $ writeIORef (l vars) (v : v')
    return ()
l %= (k, v) = do
    vars    <- ask
    liftIO $ hashInsert (l vars) k v
    return ()
l %? k = do
    vars    <- ask
    liftIO $ hashLookup (l vars) k
l %: k = return . fromJust =<< (l %? k)

l %%= kvList = do
    vars    <- ask
    liftIO $ mapM_ (\(k, v) -> writeIOArray (l vars) k v) kvList

applyToChunk :: (Rec a, ChunkBuf c n b) => (a -> x) -> c -> IOm x
applyToChunk f c = return . f . fromRecord =<< (liftIO . chunkToRecord) c

withChunk :: (ChunkBuf a n b, MonadIO m) => a -> (forall r. (Rec r) => r -> m x) -> m x
withChunk c = chunkApply (fst . chunkDecon $ c) c

splitRecords :: (ChunkBuf c n b, Typeable t) => t -> [c] -> [[c]]
splitRecords t = groupBy (const $ not . (~~ t))

findRecord :: (a -> Bool) -> [Record a] -> a
findRecord f = fromRecord . fromJust . find (f . fromRecord)

matchRecord :: (RecData a b, Eq c) => c -> (b -> c) -> a -> b
matchRecord n f = findRecord ((n ==) . f) . readData

fromA :: (Binary a, Storable a) => a -> IOm String
fromA item = liftIO $ do
    bh  <- newBinBuf (sizeOf item)
    put bh item
    seekBin bh 0
    return . trim =<< fromAStr =<< get bh

trim :: String -> String
trim = takeWhile $ not . isSpace

catBuf :: (Buf a) => a -> a -> IOm a
catBuf b1 b2 = liftIO $ do
    pstr    <- mallocForeignPtrBytes len
    withForeignPtr pstr $ \cstr -> do
        when (l1 > 0) $
            withForeignPtr p1 $ \c1 -> copyBytes cstr c1 l1
        when (l2 > 0) $
            withForeignPtr p2 $ \c2 -> copyBytes (plusPtr cstr l1) c2 l2
    return $ bufFromPStrLen (pstr, len)
    where
    len      = l1 + l2
    (p1, l1) = bufToPStrLen b1
    (p2, l2) = bufToPStrLen b2

subBuf :: (Buf a, Integral b, Integral c) => a -> b -> c -> IOm a
subBuf buf pos len = liftIO $ do
    withForeignPtr pstr $ \cstr -> do
        pstr' <- newForeignPtr_ (plusPtr cstr $ fromEnum pos)
        addFinalizer pstr' $ touchForeignPtr pstr -- ???
        return $ bufFromPStrLen (pstr', fromEnum len)
    where
    (pstr, _) = bufToPStrLen buf -- XXX - check size bounds?

subBufs :: (Buf a, Integral b, Integral c) => [a] -> b -> c -> IOm a
subBufs (b:bs) pos len = do
    if (pos <= len') then
        subBuf b pos len
        else
        subBufs bs (pos - len') len
    where
    len' = toEnum $ snd $ bufToPStrLen b -- XXX - check size bounds?

showBitmap :: (Integral i, Show a) => [a] -> i -> IOm ()
showBitmap [] _ = return ()
showBitmap bitmap n = do
    liftIO $ putStrLn . concatMap hex2bin . concatMap show $ genericTake n bitmap
    showBitmap (genericDrop n bitmap) n

hex2bin '0' = "...."
hex2bin '1' = "...@"
hex2bin '2' = "..@."
hex2bin '3' = "..@@"
hex2bin '4' = ".@.."
hex2bin '5' = ".@.@"
hex2bin '6' = ".@@."
hex2bin '7' = ".@@@"
hex2bin '8' = "@..."
hex2bin '9' = "@..@"
hex2bin 'A' = "@.@."
hex2bin 'B' = "@.@@"
hex2bin 'C' = "@@.."
hex2bin 'D' = "@@.@"
hex2bin 'E' = "@@@."
hex2bin 'F' = "@@@@"

fromA8 :: A8 -> String
fromA8 w = [
    (ebc2asc ! fromIntegral (w `shiftR` 56)),
    (ebc2asc ! fromIntegral ((w `shiftR` 48) .&. 0xff)),
    (ebc2asc ! fromIntegral ((w `shiftR` 40) .&. 0xff)),
    (ebc2asc ! fromIntegral ((w `shiftR` 32) .&. 0xff)),
    (ebc2asc ! fromIntegral ((w `shiftR` 24) .&. 0xff)),
    (ebc2asc ! fromIntegral ((w `shiftR` 16) .&. 0xff)),
    (ebc2asc ! fromIntegral ((w `shiftR` 8)  .&. 0xff)),
    (ebc2asc ! fromIntegral (w .&. 0xff))
    ]

warn :: String -> IOm ()
warn [] = return ()
warn s  = liftIO $ do
    hPutStrLn stderr $ "*** Warning: " ++ s

die :: String -> IOm a
die s = liftIO $ do
    hPutStrLn stderr $ "*** Error: " ++ s
    exitFailure

reqArg a b c d e = Option a b (ReqArg e c) d
noArg  a b c d   = Option a b (NoArg d) c

showUsage options info arg = do
    prg <- getProgName
    let banner = (`usageInfo` options) $
                 info prg ++ "\n\n" ++
                 "Options:"
    if (null arg) then do
        putStrLn banner
        exitWith ExitSuccess
        else die $ arg ++ "\n\n" ++ banner

requiredOpt :: (String -> IO a) -> String -> a
requiredOpt usage r = unsafePerformIO $ do
    usage $ "missing argument: --" ++ r

