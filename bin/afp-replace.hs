{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP

type Map = [([N1], [N1])]
type Maps = IORef [Map]
type OptsIO a = StateIO Opts a
type WriterOptsIO a = WriterStateIO Opts a

main :: IO ()
main = do
    opts    <- getOpts
    fms     <- readMaps opts
    cs      <- readInputAFP opts
    fh      <- openOutputAFP opts
    bh      <- openBinIO_ fh
    mapref	<- newIORef []
    scflref <- newIORef []
    sidref  <- newIORef 1
    runReaderT (stateMain bh cs) opts
        { currentMap = mapref
        , scflStack = scflref
        , scflID = sidref } { maps = fms }
    hClose fh

stateMain :: BinHandle -> [AFP_] -> ReaderT Opts IO ()
stateMain bh = do
    mapM_ ((liftIO . put bh =<<) . pageHandler)
        . splitRecords _PGD

pageHandler page = do
    ptxList <- sequence [ ptx_Chunks `applyToChunk` c | c <- page, c ~~ _PTX ]
    trnList <- sequence [ ptx_trn `applyToChunk` c | c <- concat ptxList, c ~~ _PTX_TRN ]
    strList <- mapM fromNStr trnList
    -- check each strList against each map element
    -- if one matches the length, return the munged page, and nix the map from mapList
    fms     <- readVar maps
    case find (matchMap strList) fms of
        Nothing -> return page
        Just fm -> do
            verbose $$ "Matched..."
            currentMap $= mungeMap $ fm
            maps $= delete fm $ fms
            mungePage page

mungeMap :: Map -> Map
mungeMap = concatMap mungePair

mungePair :: ([N1], [N1]) -> Map
mungePair (key, val) = splitChunks key `zip` (val:repeat [])

pretty :: [[N1]] -> [String]
pretty = map foo
    where
    foo = map (chr . fromEnum)
matchMap :: [[N1]] -> Map -> Bool
matchMap strList fm
    -- | trace (unlines $ pretty matched) True
    -- | trace (show (length matched, length keys)) True
    = (length matched >= length keys)
    where
    matched = filter matchOne strList
    keys = concatMap splitChunks keysList
    keysList = map fst fm
    matchOne str
        | str `elem` keys
        = True
        | length str < 2
        = False
        | last str == 0x40
        , last (init str) == 0xA1
        = matchOne (init (init str))
        | otherwise
--        , trace (map (chr . fromEnum) str) True
        = False

splitChunks = foldr joinChunks [] . strChunks

joinChunks :: [N1] -> [[N1]] -> [[N1]]
joinChunks xs [] = [xs]
joinChunks [x] ((y:ys):rest)        | y <= 0x80 = (x:y:ys):rest
joinChunks [x1,x2] ((y:ys):rest)    | y >= 0x80 = (x1:x2:y:ys):rest
joinChunks xs rest = xs:rest

strChunks :: [N1] -> [[N1]]
strChunks [] = []
strChunks (hi:lo:xs) | hi >= 0x80 = ([hi, lo] : strChunks xs)
strChunks (x:xs) = [x] : strChunks xs

mungePage :: [AFP_] -> OptsIO [AFP_]
mungePage page = do
    page ==>
        [ _MCF ... mcfHandler
        , _PTX === (`filterChunks`
            [ _PTX_TRN === trnHandler
            , _PTX_SCFL === scflHandler
            ])
        ]

-- | Record font Id to Name mappings in MCF's RLI and FQN chunks.
mcfHandler :: MCF -> OptsIO ()
mcfHandler r = do
    readChunks r ..>
        [ _MCF_T ... \mcf -> do
            fnt <- asks font
            let cs = readChunks mcf
            ids   <- sequence [ t_rli `applyToChunk` c | c <- cs, c ~~ _T_RLI ]
            fonts <- sequence [ t_fqn `applyToChunk` c | c <- cs, c ~~ _T_FQN ]
            let alist = map fromA8 fonts `zip` ids
            case lookup fnt alist of
                Just sid -> do
                    verbose $$ ("Found font ID for " ++ fnt ++ ": " ++ (show sid))
                    scflID $= id $ sid
                    return ()
                Nothing -> return ()
        ]

scflHandler :: PTX_SCFL -> WriterOptsIO ()
scflHandler r = do
    scfls <- readVar scflStack
    scflStack $= id $ (r:scfls)

trnHandler :: PTX_TRN -> WriterOptsIO ()
trnHandler r = do
    trnOld  <- fromNStr $ ptx_trn r
    fm		<- readVar currentMap
    scfls   <- readVar scflStack
    sid     <- readVar scflID
    case fm of
        ((trn, rv):rest) | trn == trnOld -> do
            -- verbose $$ map (chr . fromEnum) trn
            currentMap $= id $ rest
            case scfls of
                []      -> do
                    let rst = (ptx_trn_Type r `mod` 2)
                        typ = (ptx_scfl_Type _PTX_SCFL)
                    push _PTX_SCFL{ ptx_scfl = sid, ptx_scfl_Type = typ + rst }
                    push _PTX_SCFL{ ptx_scfl = sid, ptx_scfl_Type = typ + (1-rst) }
                (s:ss)  -> mapM_ push $ reverse (s{ ptx_scfl = 1 }:ss)
            unless (null rv) $ do
                trn' <- toNStr rv
                verbose $$ ("From:[" ++ map (chr . fromEnum) trnOld ++ "]")
                verbose $$ ("To:  [" ++ map (chr . fromEnum) rv ++ "]")
                push r { ptx_trn = trn' }
                return ()
        _ -> do
            mapM_ push $ reverse scfls
            push r
    scflStack $= id $ []

foo nstr = snd $ bufToPStrLen nstr

usage :: String -> IO a
usage = showUsage options showInfo
    where
    showInfo prg = 
        "Usage: " ++ prg ++ " -m map.txt -i input.afp -o output.afp\n"

data Opts = Opts
    { readMaps          :: IO Maps
    , maps              :: Maps
    , currentMap		:: IORef Map
    , readInputAFP      :: IO [AFP_]
    , openOutputAFP     :: IO Handle
    , font              :: String
    , verbose           :: Bool
    , showHelp          :: IO ()
    , scflStack         :: IORef [PTX_SCFL]
    , scflID            :: IORef N1
    } deriving (Typeable)

defaultOpts = Opts
    { readMaps          = requiredOpt usage "map"
   	, currentMap		= undefined
    , maps              = requiredOpt usage "map"
    , readInputAFP      = requiredOpt usage "input"
    , openOutputAFP     = requiredOpt usage "output"
    , font              = requiredOpt usage "font"
    , verbose           = True
    , showHelp          = return ()
    , scflStack         = undefined
    , scflID            = undefined
    }

options :: [OptDescr (Opts -> Opts)]
options =
    [ reqArg "m" ["map"]            "FILE"          "Replacement map"
        (\s o -> o { readMaps       = newIORef . makeMaps =<< readFile s })
    , reqArg "i" ["input"]          "FILE"          "Input AFP file"
        (\s o -> o { readInputAFP   = readAFP s })
    , reqArg "o" ["output"]         "FILE"          "Output AFP file"
        (\s o -> o { openOutputAFP  = openBinaryFile s WriteMode })
    , reqArg "f" ["font"]           "NAME"          "Font name"
        (\s o -> o { font = s })
    , noArg  "v" ["verbose"]                        "Print progress information"
        (\o   -> o { verbose        = True })
    , noArg  "h" ["help"]                           "Show help"
        (\o   -> o { showHelp       = usage "" })
    ]

run :: IO ()
-- run = withArgs (split " " "-v -m SC27.add -i SC27.AFP -o output.afp") main
run = runWith "-v -m 1-map.txt -i 1-in.afp -o 1-out.afp -f X0FDB000"

runWith str = withArgs (split " " str) main

makeMaps :: String -> [Map]
makeMaps str = entries
    where
    entries :: [Map]
    entries = map (pair . lines) groups
    pair [] = []
    pair (a:b:[]) = [(ordList a, checkDBCS $ ordList b)]
    pair (a:b:[]:rest) = (ordList a, checkDBCS $ ordList b) : pair rest
    pair x = error $ "Bad pair: " ++ (show x)
    groups = split "\n\n--\n--\n\n" str
    ordList :: (Enum a, Enum b) => [a] -> [b]
    ordList = map (toEnum . fromEnum)
    checkDBCS [] = []
    checkDBCS (x:y:xs) | x > 0x7F = (x:y:checkDBCS xs)
    checkDBCS x = error $ "Not DBCS: " ++ ordList x

gun = run

getOpts :: IO Opts
getOpts = do
    args <- getArgs
    (optsIO, rest, errs) <- return . getOpt Permute options $ procArgs args
    return $ foldl (flip ($)) defaultOpts optsIO
    where
    procArgs []     = ["-h"]
    procArgs xs     = xs
