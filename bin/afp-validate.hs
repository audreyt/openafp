{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP
import System.Exit
import qualified Control.Exception as E (try, catch, throwIO)

main :: IO ()
main = do
    args    <- getArgs
    if null args then error "Usage: afp-validate file.afp" else do
    oks <- (`mapM` args) $ \file -> do
        putStr $ file ++ ": "
        rv <- E.try $ do
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
        case rv of
            Right ok -> return ok
            Left err -> do
                print err
                return False
    if and oks
        then exitWith ExitSuccess
        else exitFailure
