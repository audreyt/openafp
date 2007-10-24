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
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

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
type WriterStateIO v a = (ChunkBuf c n b, MonadReader v m) => ChunkWriter c m a

instance Storable NStr where
    alignment _ = 8
    sizeOf      = S.length . packBuf

nullForeignPtr = unsafePerformIO (newForeignPtr_ nullPtr)

_NStr :: NStr
_NStr = mkBuf S.empty

fromAStr :: AStr -> String
fromAStr = C.unpack . S.map (ebc2ascW8 !) . packBuf

toAStr :: String -> AStr
toAStr = mkBuf . S.pack . map (asc2ebcW8 !)

fromNStr :: NStr -> [N1]
fromNStr = map N1 . S.unpack . packBuf

toNStr :: [N1] -> NStr
toNStr = mkBuf . S.pack . map fromN1

newtype ChunkType = MkChunkType Int
    deriving (Show, Eq, Typeable, Ord)

typeInt :: TypeRep -> Int
typeInt x = unsafePerformIO (typeRepKey x)

chunkTypeOf :: Typeable a => a -> ChunkType
chunkTypeOf = MkChunkType . typeInt . typeOf

-- | The ChunkBuf class represents non-parsed chunks, constructed from a
--   (ChunkType, Buffer) tuple.
class (Show c, Typeable c, Buf b, Num n, Enum n) => ChunkBuf c n b | c -> n, c -> b where
    chunkApply :: n -> c -> (forall a. (Rec a) => (a -> x)) -> x
    mkChunk :: n -> b -> c
    mkChunk = curry chunkCon
    chunkCon :: (n, b) -> c
    chunkCon = uncurry mkChunk
    chunkDecon :: c -> (n, b)
    chunkType :: c -> ChunkType
    chunkType c = chunkTypeLookup c . fst $ chunkDecon c
    chunkTypeLookup :: c -> n -> ChunkType
    packChunk :: c -> PStringLen
    chunkMapFiltersM_ :: (Monad m) => c -> [(ChunkType, c -> m [c])] -> m ()
    chunkMapFiltersM_ c possibleFilters = mapM_ (\(_, f) -> f c) filters
        where
        filters = filter (\(t, _) -> (t == chunkType c)) possibleFilters
    chunkMapFiltersM :: (Monad m) => c -> [(ChunkType, c -> m [c])] -> m [c]
    chunkMapFiltersM c possibleFilters = foldM applyF [c] filters
        where
        filters = filter (\(t, _) -> (t == chunkType c)) possibleFilters
        applyF r (_, f) = liftM concat (mapM f r)
    chunksMapFiltersM :: (Monad m) => [c] -> [(ChunkType, c -> m [c])] -> m [c]
    chunksMapFiltersM cs list = liftM concat (mapM (`chunkMapFiltersM` list) cs)
    chunksMapFiltersM_ :: (Monad m) => [c] -> [(ChunkType, c -> m [c])] -> m ()
    chunksMapFiltersM_ cs list = mapM_ (`chunkMapFiltersM_` list) cs
    decodeChunk :: (Binary (Record r)) => c -> r
    decodeChunk c = fromRecord (decode (L.fromChunks [packChunk c]))
    encodeChunk :: (Binary r, Storable r, Rec r) => r -> c
    encodeChunk item = mkChunk (toEnum (recType item)) bs
        where
        bs = mkBuf $ S.concat (L.toChunks (encode item))
    packChunk c = packBuf buf where (_, buf) = chunkDecon c

-- | The RecChunk class unifies a Rec (parent) with its contained
--   chunk types (children).
class (Rec r, ChunkBuf c n b) => RecChunk r c n b | r -> c where
    readChunks :: r -> [c]
    readChunks = error "readChunks not defined"
    writeChunks :: (Monad m) => r -> m [c] -> m r
    writeChunks = error "writeChunks not defined"

-- | The RecData class unifies a Rec (parent) with its contained
--   Rec data type (children).
class (Rec a, Rec b) => RecData a b | a -> b, b -> a where
    readData :: a -> [Record b]
    readData = error "readData not defined"
    writeData :: a -> [Record b] -> a
    writeData = error "writeData not defined"

instance (Rec a, Binary a) => Storable [a] where
    alignment = undefined
    sizeOf r = recSizeOf r
    
(~~) :: (ChunkBuf c n b, Typeable t) => c -> t -> Bool
c ~~ t = (chunkTypeOf t == chunkType c)

(<~~) :: (Monad m, ChunkBuf c n b, Typeable t, Rec r) => t -> [c] -> m r
t <~~ cs = case find (~~ t) cs of
    Just c  -> return (decodeChunk c)
    _       -> fail $ "Cannot find locate chunk: " ++ show (typeOf t, cs)

(~~>) :: (Monad m, ChunkBuf c n b, Typeable t, Rec r) => [c] -> t -> m r
(~~>) = flip (<~~)

(==>) :: (ChunkBuf c n b, Monad m) => [c] -> [(ChunkType, c -> m [c])] -> m [c]
cs ==> fs = length cs `seq` chunksMapFiltersM cs fs

(<==) :: (ChunkBuf c n b, Monad m) => [(ChunkType, c -> m [c])] -> [c] -> m [c]
(<==) = flip (==>)

(..>) :: (ChunkBuf c n b, Monad m) => [c] -> [(ChunkType, c -> m [c])] -> m ()
cs ..> fs = length cs `seq` chunksMapFiltersM_ cs fs

(<..) :: (ChunkBuf c n b, Monad m) => [(ChunkType, c -> m [c])] -> [c] -> m ()
(<..) = flip (..>)

t === f = (chunkTypeOf t, processChunk t f)
-- t ==== f = (chunkTypeOf t, processChunk t (lift . f))
-- t ===== f = (chunkTypeOf t, processChunk t (lift . lift . f))

processChunk :: (Monad m, Rec r, ChunkBuf c n b) =>
    r -> (r -> ChunkWriter c m a) -> (c -> m [c])
processChunk _ f c = do
    -- pass it to the filter along with the push function
    (_, ChunkQueue cs)  <- runWriterT (f (decodeChunk c))
    -- collect the pushed stuff
    return cs

t ... f = (chunkTypeOf t, inspectChunk t f)
t .... f = (chunkTypeOf t, inspectChunk t (lift . f))
t ..... f = (chunkTypeOf t, inspectChunk t (lift . lift . f))

inspectChunk :: (Monad m, Rec a, ChunkBuf c n b) => a -> (a -> m t) -> (c -> m [c])
inspectChunk _ f c = f (decodeChunk c) >> return [c]

push :: (ChunkBuf c n b, Monad m, Rec a) => a -> ChunkWriter c m ()
push = tell . ChunkItem . encodeChunk . Record

filterChunks :: (Monad m, RecChunk r c n b, ChunkBuf c' n' b') =>
    r -> [(ChunkType, c -> ChunkWriter c' m [c])] -> ChunkWriter c' m ()
filterChunks r list = do
    push =<< (writeChunks r $ list <== readChunks r)

data ChunkQueue a = ChunkQueue [a] | ChunkItem a deriving (Show)

instance Monoid (ChunkQueue a) where
    mempty = ChunkQueue []
    mappend (ChunkItem a) (ChunkQueue b) = ChunkQueue (a:b)
    mappend (ChunkQueue a) (ChunkQueue b)     = ChunkQueue (a ++ b)
