{-# OPTIONS -O -fglasgow-exts #-}

module Main where
import OpenAFP.Prelude

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
    runReaderT (stateMain bh cs) opts{ currentMap = mapref } { maps = fms }
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
    page ==> [ _PTX === (`filterChunks` [ _PTX_TRN === trnHandler ]) ]

trnHandler :: PTX_TRN -> WriterOptsIO ()
trnHandler r = do
    trn     <- fromNStr $ ptx_trn r
    fm		<- readVar currentMap
    case fm of
        ((trn, rv):rest) -> do
            verbose $$ map (chr . fromEnum) trn
            currentMap $= id $ rest
            unless (null rv) $ do
                verbose $$ ("==> " ++ (map (chr . fromEnum) rv))
                trn' <- toNStr rv
                push _PTX_SCFL { ptx_scfl = 1 } -- XXX font
                push r { ptx_trn = trn' }
        _ -> push r
        
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
    , verbose           :: Bool
    , showHelp          :: IO ()
    } deriving (Typeable)

defaultOpts = Opts
    { readMaps          = requiredOpt usage "map"
   	, currentMap		= undefined
    , maps              = requiredOpt usage "map"
    , readInputAFP      = requiredOpt usage "input"
    , openOutputAFP     = requiredOpt usage "output"
    , verbose           = True
    , showHelp          = return ()
    }

options :: [OptDescr (Opts -> Opts)]
options =
    [ reqArg "m" ["map"]            "FILE"          "Replacement map"
        (\s o -> o { readMaps       = newIORef . makeMaps =<< readFile s })
    , reqArg "i" ["input"]          "FILE"          "Input AFP file"
        (\s o -> o { readInputAFP   = readAFP s })
    , reqArg "o" ["output"]         "FILE"          "Output AFP file"
        (\s o -> o { openOutputAFP  = openBinaryFile s WriteMode })
    , noArg  "v" ["verbose"]                        "Print progress information"
        (\o   -> o { verbose        = True })
    , noArg  "h" ["help"]                           "Show help"
        (\o   -> o { showHelp       = usage "" })
    ]

run :: IO ()
-- run = withArgs (split " " "-v -m SC27.add -i SC27.AFP -o output.afp") main
run = runWith "-v -m test.add -i foo.afp -o bar.afp"

runWith str = withArgs (split " " str) main

makeMaps :: String -> [Map]
makeMaps str = entries
    where
    entries = map (pair . lines) groups
    pair [] = []
    pair (a:b:[]) = [(ordList a, ordList b)]
    pair (a:b:[]:rest) = (ordList a, ordList b) : pair rest
    pair x = error $ unwords x
    groups = split "\n\n--\n--\n\n" $ ignoreFont str
    ordList = map (toEnum . ord)

gun = run

ignoreFont str
    | take 5 str == "Font:"
    = unlines $ tail $ lines str
    | otherwise
    = str
    where
getOpts :: IO Opts
getOpts = do
    args <- getArgs
    (optsIO, rest, errs) <- return . getOpt Permute options $ procArgs args
    return $ foldl (flip ($)) defaultOpts optsIO
    where
    procArgs []     = ["-h"]
    procArgs xs     = xs
