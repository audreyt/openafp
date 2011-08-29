
-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types.Chunk
-- Copyright   :  (c) Audrey Tang 2004-2011
-- License     :  PublicDomain
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
type WriterStateIO v a = (Chunk c, MonadReader v m) => ChunkWriter c m a

instance Storable NStr where
    alignment _ = 8
    sizeOf      = S.length . packBuf

nullForeignPtr = unsafePerformIO (newForeignPtr_ nullPtr)

_NStr :: NStr
_NStr = mkBuf S.empty

packAStr :: AStr -> S.ByteString
packAStr = S.map (ebc2ascW8 !) . packBuf

fromAStr :: AStr -> String
fromAStr = C.unpack . packAStr

toAStr :: String -> AStr
toAStr = mkBuf . S.pack . map (asc2ebcW8 !)

packNStr :: NStr -> S.ByteString
packNStr = packBuf

fromNStr :: NStr -> [N1]
fromNStr = map N1 . S.unpack . packNStr

toNStr :: [N1] -> NStr
toNStr = mkBuf . S.pack . map fromN1

newtype ChunkType = MkChunkType Int
    deriving (Show, Eq, Typeable, Ord)

typeInt :: TypeRep -> Int
typeInt x = unsafePerformIO (typeRepKey x)

chunkTypeOf :: Typeable a => a -> ChunkType
chunkTypeOf = MkChunkType . typeInt . typeOf

-- | The Chunk class represents non-parsed chunks, constructed from a
--   (ChunkType, Buffer) tuple.
class (Show c, Typeable c, Buf (BufOf c), Enum (N c), Num (N c)) => Chunk c where
    type N c
    type BufOf c
    chunkApply :: N c -> c -> (forall a. (Rec a) => (a -> x)) -> x
    mkChunk :: N c -> BufOf c -> c
    mkChunk = curry chunkCon
    chunkCon :: (N c, BufOf c) -> c
    chunkCon = uncurry mkChunk
    chunkDecon :: c -> (N c, BufOf c)
    chunkType :: c -> ChunkType
    chunkType c = chunkTypeLookup c . fst $ chunkDecon c
    chunkTypeLookup :: c -> N c -> ChunkType
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
class (Rec r, Chunk (ChunkOf r)) => RecChunk r where
    type ChunkOf r
    readChunks :: r -> [ChunkOf r]
    readChunks = error "readChunks not defined"
    writeChunks :: Monad m => r -> m [ChunkOf r] -> m r
    writeChunks = error "writeChunks not defined"

-- Mutual dependency of Rec/Data pair.

-- | The RecData class unifies a Rec (parent) with its contained
--   Rec data type (children).
class (Rec a, Rec b, DataOf a ~ b, RecOf b ~ a) => RecData a b where
    type DataOf a
    type RecOf b
    readData :: a -> [Record b]
    readData = error "readData not defined"
    writeData :: a -> [Record b] -> a
    writeData = error "writeData not defined"

instance (Rec a, Binary a) => Storable [a] where
    alignment = undefined
    sizeOf r = recSizeOf r
    
(~~) :: (Chunk c, Typeable t) => c -> t -> Bool
c ~~ t = (chunkTypeOf t == chunkType c)

(<~~) :: (Monad m, Chunk c, Typeable t, Rec r) => t -> [c] -> m r
t <~~ cs = case find (~~ t) cs of
    Just c  -> return (decodeChunk c)
    _       -> fail $ "Cannot find locate chunk: " ++ show (typeOf t, cs)

(~~>) :: (Monad m, Chunk c, Typeable t, Rec r) => [c] -> t -> m r
(~~>) = flip (<~~)

(==>) :: (Chunk c, Monad m) => [c] -> [(ChunkType, c -> m [c])] -> m [c]
cs ==> fs = length cs `seq` chunksMapFiltersM cs fs

(<==) :: (Chunk c, Monad m) => [(ChunkType, c -> m [c])] -> [c] -> m [c]
(<==) = flip (==>)

(..>) :: (Chunk c, Monad m) => [c] -> [(ChunkType, c -> m [c])] -> m ()
cs ..> fs = length cs `seq` chunksMapFiltersM_ cs fs

(<..) :: (Chunk c, Monad m) => [(ChunkType, c -> m [c])] -> [c] -> m ()
(<..) = flip (..>)

t === f = (chunkTypeOf t, processChunk t f)
-- t ==== f = (chunkTypeOf t, processChunk t (lift . f))
-- t ===== f = (chunkTypeOf t, processChunk t (lift . lift . f))

processChunk :: (Monad m, Rec r, Chunk c) =>
    r -> (r -> ChunkWriter c m a) -> (c -> m [c])
processChunk _ f c = do
    -- pass it to the filter along with the push function
    (_, ChunkQueue cs)  <- runWriterT (f (decodeChunk c))
    -- collect the pushed stuff
    return cs

t ... f = (chunkTypeOf t, inspectChunk t f)
t .... f = (chunkTypeOf t, inspectChunk t (lift . f))
t ..... f = (chunkTypeOf t, inspectChunk t (lift . lift . f))

inspectChunk :: (Monad m, Rec a, Chunk c) => a -> (a -> m t) -> (c -> m [c])
inspectChunk _ f c = f (decodeChunk c) >> return [c]

push :: (Chunk c, Monad m, Rec a) => a -> ChunkWriter c m ()
push = tell . ChunkItem . encodeChunk . Record

filterChunks :: (Monad m, RecChunk r, Chunk c) =>
    r -> [(ChunkType, ChunkOf r -> ChunkWriter c m [ChunkOf r])] -> ChunkWriter c m ()
filterChunks r list = do
    push =<< (writeChunks r $ list <== readChunks r)

data ChunkQueue a = ChunkQueue [a] | ChunkItem a deriving (Show)

instance Monoid (ChunkQueue a) where
    mempty = ChunkQueue []
    mappend (ChunkItem a)  (ChunkItem b)  = ChunkQueue [a, b]
    mappend (ChunkItem a)  (ChunkQueue b) = ChunkQueue (a:b)
    mappend (ChunkQueue a) (ChunkQueue b) = ChunkQueue (a ++ b)
    mappend (ChunkQueue a) (ChunkItem b)  = ChunkQueue (a ++ [b])
