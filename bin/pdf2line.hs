{-# OPTIONS_GHC -O2 -fglasgow-exts -funbox-strict-fields #-}

module Main where
import Data.Binary
import Data.Binary.Get
import System.Process
import Control.Monad
import Control.Applicative
import Data.IORef
import System.IO
import System.IO.Unsafe
import System.Environment (getArgs)
import System.Environment.FindBin
import Data.ByteString (ByteString)
-- import Debug.Trace
import qualified Data.IntMap as IM
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C

__AlignRight__ :: Bool
__AlignRight__ = False

main :: IO ()
main = do
    args <- getArgs
    hSetBinaryMode stdout True
    case args of
        []  -> putStrLn "Usage: pdf2line input.pdf ... > output.txt"
        ["-"] -> do
            hSetBinaryMode stdin True
            res <- L.getContents
            mapM_ dumpPage (fromDoc $! decode res)
        _   -> forM_ args $! \inFile -> do
            -- __Bin__
            (_,out,err,pid) <- runInteractiveCommand $! "pdfdump \"" ++ inFile ++ "\""
            res <- L.hGetContents out
            L.length res `seq` waitForProcess pid
            when (L.null res) $! do
                L.hPutStr stderr =<< L.hGetContents err
            mapM_ dumpPage (fromDoc $! decode res)

dumpPage :: Page -> IO ()
dumpPage page
    | IM.null pg    = return ()
    | otherwise     = do
        _CurrentLine <- newIORef maxBound
        forM_ (IM.toAscList pg) $! \(lineNum, MkLine pt strs) -> do
            linePrev <- readIORef _CurrentLine
            replicateM_ ((lineNum - linePrev + (pt `div` 4)) `div` pt) (S.putStr _NewLine)
            _CurrentColumn <- newIORef 0
            forM_ (IM.toAscList strs) $! \(col, str) -> do
                cur <- readIORef _CurrentColumn
                S.putStr $! S.take (col - cur) _Spaces
                S.putStr str
                writeIORef _CurrentColumn (col + S.length str)
            S.putStr _NewLine
            writeIORef _CurrentLine (lineNum+pt)
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
data Line = MkLine
    { linePt    :: !Int
    , lineStrs  :: !(IM.IntMap S.ByteString)
    }
    deriving Show

instance Binary Doc where
    put = undefined
    get = MkDoc <$> getList
        where
        getList = do
            rv  <- isEmpty
            if rv then pure []
                  else liftA2 (:) get getList

data Chunk = MkChunk
    { c_right   :: !Int
    , c_upper   :: !Int
    , c_pt      :: !Int
    , c_str     :: !ByteString
    }
    deriving Show

instance Binary Page where
    put = error "put Page is not defined"
    get = getChunk maxBound []
        where
        getChunk minPt chunks = do
            rv  <- isEmpty
            if rv then done else do
                w8  <- getWord8
                case w8 of
                    0x6C    -> do -- 'l'
                        skip 1
                        col <- if __AlignRight__
                            then skip 9 *> getInt 6
                            else getInt 6 <* skip 9
                        skip 21
                        ln      <- getInt 6
                        skip 3
                        pt      <- getInt 6
                        skip 7
                        sz      <- getInt 4
                        skip 1
                        str     <- getByteString sz
                        w8'     <- getWord8
                        case w8' of
                            0x0D    -> skip 1
                            0x0A    -> pure ()
                            _       -> fail $! "Bad parse: " ++ show w8'
                        let pt' = min minPt pt
                        getChunk pt' (MkChunk col ln pt str:chunks)
                    0x0D    -> skip 1 *> done
                    0x0A    -> done
                    _       -> fail $! "Bad parse: " ++ show w8
            where
            done = pure $! pageOf (foldl (buildPage minPt) (MkBuild (MkPage IM.empty) 0) chunks)
        getInt :: Int -> Get Int
        getInt (n+1) = liftA2 mkInt getWord8 (getInt n)
            where
            mkInt digit rest = fromEnum (digit - 0x30) * (10 ^ n) + rest
        getInt _     = pure 0
        buildPage minPt (MkBuild (MkPage pg) base) (MkChunk col ln pt str)
            = MkBuild (MkPage (IM.insert base' entry pg)) base'
            where
            sz      = S.length str
            width   = if __AlignRight__
                then ((col * 2) `div` minPt) - sz
                else ((col * 2) `div` minPt)
            base'   = if abs (ln - base) + (minPt `div` 4) < minPt then base else ln
            entry   = case IM.lookup base' pg of
                Just (MkLine pt' strs)  -> MkLine (max pt pt') (IM.insert width str strs)
                _                       -> MkLine pt (IM.singleton width str)

data Build = MkBuild
    { pageOf    :: !Page
    , baseOf    :: !Int
    }
    deriving Show

instance Applicative Get where
    pure = return
    (<*>) = ap
