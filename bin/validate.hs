{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP.Prelude
import System.Exit

main :: IO ()
main = do
    args    <- getArgs
    if null args then error "Usage: validate file.afp" else do
    oks <- (`mapM` args) $ \file -> do
        putStr $ file ++ ": "
        cs <- readAFP file
        let (bpg, epg) = foldr count (0, 0) cs
            count chunk (b, e) | chunk ~~ _BPG = (b+1, e)
            count chunk (b, e) | chunk ~~ _EPG = (b, e+1)
            count _ pair = pair
            ok = (bpg > 0 && bpg == epg)
        putStrLn $ concat
            [ show bpg, " BPG, "
            , show epg, " EPG ("
            , if ok then "OK" else "FAIL"
            , ")"
            ]
        return ok
    if and oks
        then exitWith ExitSuccess
        else exitFailure
