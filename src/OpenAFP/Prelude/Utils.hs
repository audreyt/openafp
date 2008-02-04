{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Prelude.Utils
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
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
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L

import Data.Int
import GHC.Base (build, unsafeChr, realWorld#)
import GHC.IOBase (IO(..))

hashByteString (S.PS x s l) = inlinePerformIO $ withForeignPtr x $ \p ->
     go (0 :: Int32) (p `plusPtr` s) l
     where
     go :: Int32 -> Ptr Word8 -> Int -> IO Int32
     go a b c | a `seq` b `seq` c `seq` False = undefined
     go h _ 0 = return h
     go h p n = do w <- peek p
                   go (fromIntegral w + rotateL h 8) (p `plusPtr` 1) (n-1)

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

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
l %: k = return . fromJust'' =<< (l %? k)

l %%= kvList = do
    vars    <- ask
    liftIO $ mapM_ (\(k, v) -> writeIOArray (l vars) k v) kvList

applyToChunk :: (Monad m, Rec a, ChunkBuf c n b) => (a -> x) -> c -> m x
applyToChunk f = return . f . decodeChunk

withChunk :: (ChunkBuf a n b) => a -> (forall r. (Rec r) => r -> x) -> x
withChunk c = chunkApply (fst . chunkDecon $ c) c

splitRecords :: (ChunkBuf c n b, Typeable t) => t -> [c] -> [[c]]
splitRecords t = groupBy (const $ not . (~~ t))

findRecord :: (a -> Bool) -> [Record a] -> a
findRecord f = fromRecord . fromJust' . find (f . fromRecord)

fromJust' (Just x) = x
fromJust' Nothing = error "fromJust1 - fail"

fromJust'' (Just x) = x
fromJust'' Nothing = error "fromJust2 - fail"

matchRecord :: (RecData a b, Eq c) => c -> (b -> c) -> a -> b
matchRecord n f = findRecord ((n ==) . f) . readData

matchRecordMaybe :: (RecData a b, Eq c) => c -> (b -> c) -> a -> Maybe b
matchRecordMaybe n f = findRecordMaybe ((n ==) . f) . readData

findRecordMaybe :: (a -> Bool) -> [Record a] -> Maybe a
findRecordMaybe f = maybe Nothing (Just . fromRecord) . find (f . fromRecord)

fromA :: (Binary a, Storable a) => a -> String
fromA = trim . fromAStr . mkBuf . S.concat . L.toChunks . encode

trim :: String -> String
trim = takeWhile $ not . isSpace

catBuf :: Buf a => a -> a -> a
catBuf b1 b2 = mkBuf (packBuf b1 `S.append` packBuf b2)

subBuf :: (Buf a, Integral b, Integral c) => a -> b -> c -> a
subBuf buf pos len = mkBuf (S.take (fromIntegral len) (S.drop (fromIntegral pos) (packBuf buf)))

subBufs :: (Buf a, Integral b, Integral c) => [a] -> b -> c -> a
subBufs (b:bs) pos len
    | pos <= len' = subBuf b pos len
    | otherwise   = subBufs bs (pos - len') len
    where
    len' = fromIntegral . S.length $ packBuf b

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

toA8 :: String -> A8
toA8 s = sum
    [ n1 `shiftL` 56, n2 `shiftL` 48, n3 `shiftL` 40, n4 `shiftL` 32
    , n5 `shiftL` 24, n6 `shiftL` 16, n7 `shiftL` 8 , n8
    ]
    where
    [n1, n2, n3, n4, n5, n6, n7, n8] = map (fromIntegral . (asc2ebc !) . ord) padded
    padded = take 8 (s ++ repeat ' ')

packA8 :: A8 -> S.ByteString
packA8 w = S.unsafeCreate 8 $ \ptr -> do
    pokeByteOff ptr 0 (ebc2ascW8 ! fromIntegral (w `shiftR` 56))
    pokeByteOff ptr 1 (ebc2ascW8 ! fromIntegral ((w `shiftR` 48) .&. 0xff))
    pokeByteOff ptr 2 (ebc2ascW8 ! fromIntegral ((w `shiftR` 40) .&. 0xff))
    pokeByteOff ptr 3 (ebc2ascW8 ! fromIntegral ((w `shiftR` 32) .&. 0xff))
    pokeByteOff ptr 4 (ebc2ascW8 ! fromIntegral ((w `shiftR` 24) .&. 0xff))
    pokeByteOff ptr 5 (ebc2ascW8 ! fromIntegral ((w `shiftR` 16) .&. 0xff))
    pokeByteOff ptr 6 (ebc2ascW8 ! fromIntegral ((w `shiftR` 8)  .&. 0xff))
    pokeByteOff ptr 7 (ebc2ascW8 ! fromIntegral (w .&. 0xff))

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

io :: MonadIO m => IO a -> m a
io = liftIO
