{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP
import System.Exit
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    args    <- getArgs
    if null args then error "Usage: afp-page file.afp [output.afp]" else do
    cs <- readAFP $ head args
    let outputFile = case args of
            (_:fn:_)    -> fn
            _           -> "output.afp"
    cs' <- cs !==>!
        [ _MCF === (`filterChunks`
            [ _MCF_T === (`filterChunks`
                [ _T_FQN === fqnHandler
                , _T_RLI ... io . writeIORef cnt . fromEnum . t_rli
                ])
            ])
        ]
    writeAFP outputFile cs'

{-# NOINLINE cnt #-}
cnt :: IORef Int
cnt = unsafePerformIO (newIORef 0)

cs !==>! list = iter cs
    where
    iter [] = return []
    iter (c:cs) = do
        this    <- c `chunkMapFiltersM` list
        rest    <- unsafeInterleaveIO (iter cs)
        return $ this ++ rest

fqnHandler fqn = do
    c <- io $ readIORef cnt  
    let fqn' = case c of
            0           -> fqnDBCS
            35          -> fqnDBCS
            _ | isM__T  -> fqnDBCS
            _           -> fqn
    push fqn'
    where
    a       = packAStr (t_fqn fqn)
    isM__T  = (C.length a >= 6) && ((C.index a 2 == 'M') || (C.index a 5 == 'T'))
    fqnDBCS = fqn{ t_fqn = toAStr "T0XXXX  " }

