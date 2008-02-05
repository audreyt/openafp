{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP
import System.Exit
import qualified Data.ByteString.Char8 as C

-- Algorithm:
--  Gather everything up to first BPG
--  write out each BPG/EPG chunks
--  append ENG+EDT

main :: IO ()
main = do
    args    <- getArgs
    if null args then error "Usage: afp-split file.afp" else do
    let inFile = head args
    cs      <- readAFP inFile
    let (preamble:rest) = splitPages cs
        _edt      = encodeChunk $ Record _EDT
    forM_ ([0..] `zip` rest) $ \(i, page) -> do
        let outFile = inFile ++ ('.':show i)
        putStrLn outFile
        writeAFP outFile $ preamble ++ page ++ [_edt]

splitPages :: [AFP_] -> [[AFP_]]
splitPages cs = if null rest then [this] else case splitPages rest' of
    []      -> [this, rest]
    (y:ys)  -> (this:(begins ++ y):ys)
    where
    (this, rest)    = break isBeginPage cs
    (begins, rest') = span isBeginPage rest

isBeginPage :: AFP_ -> Bool
isBeginPage t = (t ~~ _BPG) || (t ~~ _BNG)
