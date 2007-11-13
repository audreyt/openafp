{-# OPTIONS_GHC -O2 -fglasgow-exts #-}

module Main where
import Data.Binary
import Data.Binary.Get
import Control.Monad
import Data.IORef
import System.IO
import System.IO.Unsafe
import System.Environment (getArgs)
import Data.ByteString (ByteString)
import qualified Data.IntMap as IM
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

main = do
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    res     <- L.getContents
    mapM_ dumpPage (fromDoc $ decode res)

dumpPage page
    | IM.null pg    = return ()
    | otherwise     = do
        forM_ (IM.elems pg) $ \line -> do
            _CurrentColumn <- newIORef 0
            forM_ (IM.toAscList line) $ \(col, str) -> do
                cur <- readIORef _CurrentColumn
                S.putStr $ S.take (col - cur) _Spaces
                S.putStr str
                writeIORef _CurrentColumn (col + S.length str)
            S.putStr _NewLine
        S.putStr _NewPage
    where
    pg = fromPage page

_Spaces, _NewLine, _NewPage :: ByteString
_Spaces  = S.replicate 4096 0x20
_NewLine = C.pack "\r\n"
_NewPage = C.pack "\r\n\x0C\r\n"

-- A Page is a IntMap from line-number to a map from column-number to bytestring.
newtype Doc = MkDoc { fromDoc :: [Page] } deriving Show
newtype Page = MkPage { fromPage :: IM.IntMap Line } deriving Show
type Line = IM.IntMap S.ByteString

instance Binary Doc where
    put = undefined
    get = liftM MkDoc getList
        where
        getList = do
            rv  <- isEmpty
            if rv then return [] else do
                x   <- get
                xs  <- getList
                return (x:xs)

data Chunk = MkChunk
    { c_right   :: !Int
    , c_upper   :: !Int
    , c_str     :: !ByteString
    }

instance Binary Page where
    put = undefined
    get = getChunk 0 []
        where
        getChunk minFont chunks = do
            rv  <- isEmpty
            if rv then done else do
                w8  <- getWord8
                case w8 of
                    0x6C    -> do -- 'l'
                        skip 10
                        col'    <- getInt 6
                        skip 21
                        ln      <- getInt 6
                        skip 3
                        font    <- getInt 6
                        skip 7
                        sz      <- getInt 4
                        skip 1
                        str     <- getByteString sz
                        w8'     <- getWord8
                        case w8' of
                            0x0D    -> skip 1
                            0x0A    -> return ()
                            _       -> fail $ "Bad parse: " ++ show w8'
                        let font' = if minFont == 0 then font else min minFont (font `div` 2)
                        getChunk font' (MkChunk col' ln str:chunks)
                    0x0D    -> skip 1 >> done
                    0x0A    -> done
                    _       -> fail $ "Bad parse: " ++ show w8
            where
            done = return $ foldl (buildPage minFont) (MkPage IM.empty) chunks 
        getInt 0 = return 0
        getInt n = do
            digit   <- getWord8
            rest    <- getInt (n-1)
            return $ (fromEnum $ digit - 0x30) * (10 ^ (n-1)) + rest
        buildPage minFont (MkPage pg) (MkChunk col' ln' str) = MkPage $ IM.insert ln entry pg
            where
            sz      = S.length str
            width   = (col' `div` minFont) - sz
            ln      = ln' `div` minFont
            entry   = case IM.lookup ln pg of
                Nothing     -> IM.singleton width str
                Just    im  -> IM.insert width str im

