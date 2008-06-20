{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP
import System.Exit
import Data.Char (isDigit, isAlphaNum)
import Data.List (find)
import qualified Data.ByteString.Char8 as C

-- Algorithm:
--  Gather everything up to first BPG
--  write out each BPG/EPG chunks
--  append ENG+EDT

main :: IO ()
main = do
    args    <- getArgs
    if null args then error "Usage: afp-split-scb file.afp [pages]" else do
    let (inFile:outPages) = args
    cs      <- readAFP inFile
    let (preamble:rest) = splitPages cs
        _eng      = encodeChunk $ Record _ENG
        _edt      = encodeChunk $ Record _EDT
        pagePairs = map show [1..] `zip` rest
    if null outPages
        then forM_ pagePairs $ \(i, page) -> do
            let Just tle      = find (~~ _TLE) page
                Just (_:av:_) = tle_Chunks `applyToChunk` tle
                Just str      = t_av `applyToChunk` av
                pgs           = filter (~~ _BPG) page
                --
                Just seg      = find (~~ _IPS) page
                Just name     = ips `applyToChunk` seg
                --
                outFile = inFile
                    ++ ('.':filter isDigit (fromAStr str))
                    ++ ('.':takeWhile isAlphaNum (fromAStr name))
                    ++ ('.':i)
                    ++ ('.':show (length pgs))

            putStrLn outFile
            writeAFP outFile $ preamble ++ page ++ [_eng, _edt]
        else do
            let outFile = inFile ++ ".part"
            writeAFP outFile $ preamble ++ concat [ page | (i, page) <- pagePairs, i `elem` outPages ] ++ [_eng, _edt]
            putStrLn outFile

splitPages :: [AFP_] -> [[AFP_]]
splitPages cs = if null rest then [this] else case splitPages rest' of
    []      -> [this, rest]
    (y:ys)  -> (this:(begins ++ y):ys)
    where
    (this, rest)    = break isBeginPage cs
    (begins, rest') = span isBeginPage rest

isBeginPage :: AFP_ -> Bool
isBeginPage t = (t ~~ _BNG)
