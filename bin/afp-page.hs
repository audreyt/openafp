{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP
import Data.ByteString as S (findIndex, take, drop)
import Data.ByteString.Internal (fromForeignPtr)
import System.Exit

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
        , _MCF1 === mcf1Handler
        , _PTX  === (`filterChunks`
            [ _PTX_TRN  === trnHandler
            , _PTX_SCFL ... io . writeIORef currentFont
            ])
        ]
    writeAFP outputFile cs'

cs !==>! list = iter cs
    where
    iter [] = return []
    iter (c:cs) = do
        this    <- c `chunkMapFiltersM` list
        rest    <- unsafeInterleaveIO (iter cs)
        return $ this ++ rest

data TrnMode = Nil | SBCS | DBCS deriving (Show, Eq, Enum)

-- We now have a trn.  We need to break it into DBCS and SBCS parts;
-- if we're in a SBCS part, any thing in 0x41-0x7F goes to DBCS.
-- if we're in a DBCS part, 0x40 needs one-char lookahead, and everything else goes to SBCS.
trnHandler trn = scanTrn 0 0 Nil
    where
    bs@(PS pstr off len) = packBuf $ ptx_trn trn
    pstr'                = castForeignPtr pstr
    cstr                 = unsafeForeignPtrToPtr pstr' `plusPtr` off
    scanTrn i prev mode
        | i == len = case mode of
            Nil     -> push trn
            SBCS    -> emit
            DBCS    -> emit >> (io $ readIORef currentFont) >>= push
        | otherwise = io (peekElemOff cstr i :: IO Word8) >>= \ch -> case mode of
            Nil     -> {-# SCC "Nil" #-} case isDBCS ch of
                DBCS    -> case i `mod` 2 of
                    0   -> scanTrn (i+2) 0 DBCS 
                    _   -> emit >> scanTrn (i+2) i DBCS
                SBCS    -> scanTrn (i+1) 0 SBCS -- len 0 Nil -- Begun with SBCS -- Skip to end
                Nil     -> scanTrn (i+1) 0 Nil
            SBCS    -> {-# SCC "SBCS" #-} ($ isSBCS ch) . fix $ \redo mch -> case mch of
                DBCS    -> emit >> scanTrn (i+2) i DBCS
                SBCS    -> scanTrn (i+1) prev SBCS
                Nil     -> let i' = i+1 in if i' == len then redo SBCS else do
                    ch' <- io (peekElemOff cstr i' :: IO Word8)
                    redo (if (ch' >= 0x81) && (ch' <= 0xF9) &&
                       ((ch' <= 0x89 || ch' >= 0xF0) ||
                        (ch' >= 0x91 && ch' <= 0x99) ||
                        (ch' >= 0xA2 && ch' <= 0xA9) ||
                        (ch' >= 0xC1 && ch' <= 0xC9) ||
                        (ch' >= 0xD1 && ch' <= 0xD9) ||
                        (ch' >= 0xE2 && ch' <= 0xE9)) then SBCS else DBCS)
            DBCS    -> {-# SCC "DBCS" #-} ($ isDBCS ch) . fix $ \redo mch -> case mch of
                Nil     -> let i' = i+1 in if i' == len then redo SBCS else do
                    ch' <- io (peekElemOff cstr i' :: IO Word8)
                    redo (case ch' :: Word8 of
                        0x40    -> DBCS
                        _       -> SBCS)
                DBCS    -> scanTrn (i+2) prev DBCS
                _       -> emit >> scanTrn (i+1) i SBCS
        where
        emit = {-# SCC "emit" #-} do
            scfl    <- io $ readIORef currentFont
            case mode of
                SBCS    -> push scfl
                DBCS | prev > 0 || i /= len || isJust (S.findIndex isUnambiguous bs) -> do
                    c <- io $ readIORef cnt  
                    push scfl{ ptx_scfl = if c >= 35 then 36 else 1 }
                _       -> return ()
            push curTRN
        curTRN = {-# SCC "curTRN" #-} trn{ ptx_trn = mkBuf (S.take (i-prev) (S.drop prev bs)) }
                        
{-# INLINE isDBCS #-}
isDBCS 0x40 = Nil
isDBCS ch   = if (ch >= 0x41 && ch <= 0x7F) then DBCS else SBCS

{-# INLINE isSBCS #-}
isSBCS ch = if ch < 0x41 || ch > 0x7F then SBCS else case ch of
    0x4B -> Nil
    0x5C -> Nil
    0x60 -> Nil
    0x61 -> Nil
    0x6B -> Nil
    _    -> DBCS

{-# INLINE isUnambiguous #-}
isUnambiguous 0x40 = False
isUnambiguous ch   = case isSBCS ch of
    Nil -> False
    _   -> True

mcf1Handler mcf1 = do
    push $ writeData mcf1 (map fixMCF1 $ readData mcf1)
    where
    fixMCF1 :: Record MCF1_Data -> Record MCF1_Data
    fixMCF1 (Record mcf@MCF1_Data{ mcf1_CodedFontLocalId = 31 }) = Record mcf{ mcf1_CodedFontName = toA8 "T0XXXX" }
    fixMCF1 x = x

{-# NOINLINE currentFont #-}
currentFont :: IORef PTX_SCFL
currentFont = unsafePerformIO (newIORef _PTX_SCFL)

{-# NOINLINE cnt #-}
cnt :: IORef Int
cnt = unsafePerformIO (newIORef 0)

fqnHandler fqn = do
    c <- io $ readIORef cnt  
    let fqn' = case c of
            0  -> fqn{ t_fqn = toAStr "T0XXXX" }
            35 -> fqn{ t_fqn = toAStr "T0XXXX" }
            _  -> fqn
    push fqn'
    return ()
