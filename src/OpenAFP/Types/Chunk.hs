{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types.Chunk
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module handles pre-parsed "chunks" in AFP files.
--
-----------------------------------------------------------------------------

module OpenAFP.Types.Chunk where
import OpenAFP.Internals
import OpenAFP.Types.Buffer
import OpenAFP.Types.Record
import System.IO.Unsafe (unsafePerformIO)

infixl 4 ~~
infixl 4 <~~
infixl 4 ~~>
infixl 4 <==
infixl 4 ==>
infixl 4 <..
infixl 4 ..>
infixr 4 ===
infixr 4 ...
infixr 4 ....
infixr 4 .....

type NStr = Buffer0
type AStr = NStr

type ChunkWriter c = WriterT (ChunkQueue c)
type WriterStateIO v a = (ChunkBuf c n b, MonadReader v m, MonadPlus m, MonadIO m, MonadError e m, Show e, Typeable e) => ChunkWriter c m a

instance Storable NStr where
    alignment _ = 8
    sizeOf      = snd . bufToPStrLen

nullForeignPtr = unsafePerformIO (newForeignPtr_ nullPtr)

_NStr :: NStr
_NStr = bufFromPStrLen (nullForeignPtr, 0)

fromAStr :: AStr -> IOm String
fromAStr nstr = liftIO $ do
    let (pstr, len) = bufToPStrLen nstr
    withForeignPtr (castForeignPtr pstr) $ \cstr -> do
        ns <- peekArray len cstr
        return $ map (ebc2asc !) ns

toAStr :: String -> IOm AStr
toAStr str = liftIO $ do
    let len = length str
    pstr <- mallocForeignPtrBytes len
    withForeignPtr pstr $ \cstr -> do
        pokeArray cstr $ map ((asc2ebc !) . ord) str
        return $ bufFromPStrLen (castForeignPtr pstr, len)

fromNStr :: NStr -> IOm [N1]
fromNStr nstr = liftIO $ do
    let (pstr, len) = bufToPStrLen nstr
    withForeignPtr (castForeignPtr pstr) $ \cstr ->
        peekArray len cstr

toNStr :: [N1] -> IOm NStr
toNStr list = liftIO $ do
    let len = length list
    pstr <- mallocForeignPtrBytes len
    withForeignPtr pstr $ \cstr -> do
        pokeArray cstr list
        return $ bufFromPStrLen (castForeignPtr pstr, len)

newtype ChunkType = MkChunkType Int
    deriving (Show, Eq, Typeable, Ord)

typeInt :: TypeRep -> Int
typeInt x = unsafePerformIO (typeRepKey x)

chunkTypeOf :: Typeable a => a -> ChunkType
chunkTypeOf = MkChunkType . typeInt . typeOf

-- | The ChunkBuf class represents non-parsed chunks, constructed from a
--   (ChunkType, Buffer) tuple.
class (Show c, Typeable c, Buf b, Num n, Enum n) => ChunkBuf c n b | c -> n, c -> b where
    chunkApply :: (MonadIO m) => n -> c -> (forall a. (Rec a) => (a -> m x)) -> m x
    chunkCon :: (n, b) -> c
    chunkDecon :: c -> (n, b)
    chunkType :: c -> ChunkType
    chunkType c = chunkTypeLookup c . fst . chunkDecon $ c
    chunkTypeLookup :: c -> n -> ChunkType
    chunkToPStrLen :: c -> PStringLen
    chunkMapFiltersM_ :: (MonadIO m) => c -> [(ChunkType, c -> m [c])] -> m ()
    chunkMapFiltersM_ c possibleFilters
        | null filters  = return ()
        | otherwise     = mapM_ (\(_, f) -> do f c ; return ()) filters
        where filters = filter (\(t, _) -> (t == chunkType c)) possibleFilters
    chunkMapFiltersM :: (MonadIO m) => c -> [(ChunkType, c -> m [c])] -> m [c]
    chunkMapFiltersM c possibleFilters
        | null filters  = return [c]
        | otherwise     = do
            c' <- foldM applyF [c] filters
            return c'
        where
            filters = filter (\(t, _) -> (t == chunkType c)) possibleFilters
            applyF r (_, f) = do
                r' <- mapM f r
                return $ concat r'
    chunksMapFiltersM :: (MonadIO m) => [c] -> [(ChunkType, c -> m [c])] -> m [c]
    chunksMapFiltersM cs list = do
        lol <- mapM (`chunkMapFiltersM` list) cs
        return $ concat lol
    chunksMapFiltersM_ :: (MonadIO m) => [c] -> [(ChunkType, c -> m [c])] -> m ()
    chunksMapFiltersM_ cs list = mapM_ (`chunkMapFiltersM_` list) cs
    chunkToRecord :: (Binary (Record r)) => c -> IO (Record r)
    chunkToRecord c = do
	let (pstr, len) = chunkToPStrLen c
        bh <- openBinBuf (pstr, len)
        -- XXX: Associate this with the reified chunkType
        (Record r) <- get bh
        addFinalizer r $ touchForeignPtr pstr
        return $ Record r
    chunkToPStrLen c = bufToPStrLen buf where (_, buf) = chunkDecon c
    chunkFromRecord :: (Binary r, Storable r, Rec r) => r -> IO c
    chunkFromRecord item = do
        bh <- newBinBuf (sizeOf item)
        put bh item
        return $ chunkCon (toEnum . recType $ item, bufFromPStrLen $ bufOf bh)

-- | The RecChunk class unifies a Rec (parent) with its contained
--   chunk types (children).
class (Rec r, ChunkBuf c n b) => RecChunk r c n b | r -> c where
    readChunks :: r -> [c]
    readChunks = error "readChunks not defined"
    writeChunks :: (MonadIO m) => r -> m [c] -> m r
    writeChunks = error "writeChunks not defined"

-- | The RecData class unifies a Rec (parent) with its contained
--   Rec data type (children).
class (Rec a, Rec b) => RecData a b | a -> b, b -> a where
    readData :: a -> [Record b]
    readData = error "readData not defined"
    writeData :: a -> [Record b] -> a
    writeData = error "writeData not defined"

-- | The ChunkLookup class unifies a Chunk (parent) with its lookup
--   dispatch function.
{-

class ChunkLookup a b | a -> b where
    chunkLookup :: (ChunkBuf a c) => b -> c -> a

instance (ChunkLookup a b, ChunkBuf a c, Binary c) => Binary a where
    put bh chunk = put bh $ snd $ chunkDecon chunk
    get bh = do
        buf     <- get bh
        bh'     <- openBinBuf $ bufToPStrLen buf
        buftype <- get bh'
        return $ chunkLookup buftype buf
-}


instance (Rec a, Binary a) => Storable [a] where
    alignment = undefined
    sizeOf r = recSizeOf r
    
(~~) :: (ChunkBuf c n b, Typeable t) => c -> t -> Bool
c ~~ t = (chunkTypeOf t == chunkType c)

(<~~) :: (ChunkBuf c n b, Typeable t, Rec r) => t -> [c] -> IOm r
t <~~ cs = do
    Record r <- liftIO $ chunkToRecord $ fromJust $ find (~~ t) cs
    return r

(~~>) :: (ChunkBuf c n b, Typeable t, Rec r) => [c] -> t -> IOm r
cs ~~> t = do
    Record r <- liftIO $ chunkToRecord $ fromJust $ find (~~ t) cs
    return r

(==>) :: (ChunkBuf c n b, MonadIO m) => [c] -> [(ChunkType, c -> m [c])] -> m [c]
cs ==> fs = length cs `seq` chunksMapFiltersM cs fs

(<==) :: (ChunkBuf c n b, MonadIO m) => [(ChunkType, c -> m [c])] -> [c] -> m [c]
(<==) = flip (==>)

(..>) :: (ChunkBuf c n b, MonadIO m) => [c] -> [(ChunkType, c -> m [c])] -> m ()
cs ..> fs = length cs `seq` chunksMapFiltersM_ cs fs

(<..) :: (ChunkBuf c n b, MonadIO m) => [(ChunkType, c -> m [c])] -> [c] -> m ()
(<..) = flip (..>)

t === f = (chunkTypeOf t, processChunk t f)
-- t ==== f = (chunkTypeOf t, processChunk t (lift . f))
-- t ===== f = (chunkTypeOf t, processChunk t (lift . lift . f))

processChunk :: (MonadIO m, Rec r, ChunkBuf c n b) =>
    r -> (r -> ChunkWriter c m a) -> (c -> m [c])
processChunk _ f c = do
    -- grab a naked record
    (Record r)          <- liftIO $ chunkToRecord c
    -- pass it to the filter along with the push function
    (_, ChunkQueue cs)  <- runWriterT (f r)
    -- collect the pushed stuff
    return cs

t ... f = (chunkTypeOf t, inspectChunk t f)
t .... f = (chunkTypeOf t, inspectChunk t (lift . f))
t ..... f = (chunkTypeOf t, inspectChunk t (lift . lift . f))

inspectChunk :: (MonadIO m, Rec a, ChunkBuf c n b) => a -> (a -> m t) -> (c -> m [c])
inspectChunk _ f c = do
    (Record r)  <- liftIO $ chunkToRecord c
    f r
    return [c]

push :: (ChunkBuf c n b, MonadIO m, Rec a) => a -> ChunkWriter c m ()
push r = do
    chunk <- liftIO $ chunkFromRecord $ Record r
    tell $ ChunkItem chunk
    return ()

filterChunks :: (MonadIO m, RecChunk r c n b, ChunkBuf c' n' b') =>
    r -> [(ChunkType, c -> ChunkWriter c' m [c])] -> ChunkWriter c' m ()
filterChunks r list = do
    push =<< (writeChunks r $ list <== readChunks r)

data ChunkQueue a = ChunkQueue [a] | ChunkItem a deriving (Show)

instance Monoid (ChunkQueue a) where
    mempty = ChunkQueue []
    mappend (ChunkItem a) (ChunkQueue b) = ChunkQueue (a:b)
    mappend (ChunkQueue a) (ChunkQueue b)     = ChunkQueue (a ++ b)
