{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP

main :: IO ()
main = do
    args    <- getArgs
    if null args then error "Usage: afp-type file.afp" else do
    cs <- readAFP $ head args
    case find (~~ _ER) cs of
        Nothing -> return ()
        Just c  -> do
            resName <- er `applyToChunk` c
            putStrLn $ fromAStr resName
