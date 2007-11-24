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
    let (preamble, rest) = break (~~ _BPG) cs
        _epg      = encodeChunk $ Record _EPG
        _eng      = encodeChunk $ Record _ENG
        _edt      = encodeChunk $ Record _EDT
        postamble = [_epg, _eng, _edt]
    forM_ ([0..] `zip` splitPages rest) $ \(i, page) -> do
        let outFile = inFile ++ ('.':show i)
        putStrLn outFile
        writeAFP outFile $ preamble ++ page ++ postamble

splitPages :: [AFP_] -> [[AFP_]]
splitPages cs = case rest of
    []      -> []
    (_:xs)  -> (this:splitPages xs)
    where
    (this, rest) = break (~~ _EPG) cs
