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

pageHandler :: [AFP_] -> OptsIO [AFP_]
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
            currentMap $= id $ fm
            maps $= delete fm $ fms
            mungePage fm page

matchMap :: [[N1]] -> Map -> Bool
matchMap strList fm = (length matched == length fm)
    where
    matched = filter matchOne strList
    matchOne str
        | str `elem` map fst fm
        = True
        | length str < 2
        = False
        | last str == 0x40
        , last (init str) == 0xA1
        = matchOne $ init (init str)
        | otherwise
        = False

mungePage :: [AFP_] -> OptsIO [AFP_]
mungePage page = do
    page ==> [ _PTX === (`filterChunks` [ _PTX_TRN === trnHandler ]) ]

trnHandler :: PTX_TRN -> WriterOptsIO ()
trnHandler r = do
    trn     <- fromNStr $ ptx_trn r
    fm		<- readVar currentMap
    let rv = lookup trn fm
    trn'    <- toNStr $ maybe trn id rv
	when (isJust rv) $ do
		currentMap $= delete (trn, fromJust rv) $ fm
    push r { ptx_trn = trn' }

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
run = runWith "-v -m x.add -i x.afp -o y.afp"

runWith str = withArgs (split " " str) main

makeMaps :: String -> [Map]
makeMaps str = entries
    where
    entries = map (pair . lines) groups
    pair [] = []
    pair (a:b:[]) = [(ordList a, ordList b)]
    pair (a:b:[]:rest) = (ordList a, ordList b) : pair rest
    pair x = error $ unwords x
    groups = split "\n\n--\n--\n\n" str
    ordList = map (toEnum . ord)

getOpts :: IO Opts
getOpts = do
    args <- getArgs
    (optsIO, rest, errs) <- return . getOpt Permute options $ procArgs args
    return $ foldl (flip ($)) defaultOpts optsIO
    where
    procArgs []     = ["-h"]
    procArgs xs     = xs
