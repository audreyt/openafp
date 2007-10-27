{-# OPTIONS -O -fglasgow-exts -funbox-strict-fields -fimplicit-params #-}

module Main where
import Text.XHtml
import OpenAFP hiding ((!))
import qualified Data.Set as Set
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.HashTable as H

-- The key here is inventing a ConcreteDataView for our data structure.
-- See OpenAFP.Types.View for details.

type Encodings = [String]

data Opts = Opts
    { encodings         :: Encodings
    , inputFile         :: String
    , openOutputHandle  :: IO Handle
    , verbose           :: Bool
    , showHelp          :: IO ()
    } deriving (Typeable)

defaultOpts :: Opts
defaultOpts = Opts
    { encodings         = ["937", "500"]
    , inputFile         = requiredOpt usage "input"
    , openOutputHandle  = return stdout
    , verbose           = False
    , showHelp          = return ()
    }

usage :: String -> IO a
usage = showUsage options showInfo
    where
    showInfo prg = 
        "Usage: " ++ prg ++ " [-e enc,enc...] input.afp > output.html\n" ++
        "( example: " ++ prg ++ " -e 437,947 big5.afp > output.html)"

options :: [OptDescr (Opts -> Opts)]
options =
    [ reqArg "e" ["encodings"]      "ENC,ENC..."    "Text encodings (default: 937,500)"
        (\s o -> o { encodings          = split "," s })
    , reqArg "i" ["input"]          "FILE"          "Input AFP file"
        (\s o -> o { inputFile          = s })
    , reqArg "o" ["output"]         "FILE"          "Output HTML file"
        (\s o -> o { openOutputHandle   = openFile s WriteMode })
    , noArg  "h" ["help"]                           "Show help"
        (\o   -> o { showHelp           = usage "" })
    ]

getOpts :: IO Opts
getOpts = do
    args <- getArgs
    (optsIO, rest, errs) <- return . getOpt Permute options $ procArgs args
    return $ foldl (flip ($)) defaultOpts optsIO
    where
    procArgs xs
	| null xs	    = ["-h"]
	| even $ length xs  = xs
	| otherwise	    = init xs ++ ["-i", last xs]

run :: IO ()
run = withArgs (split " " "-e 937,500 -i ln-1.afp -o x.html") main

main :: IO ()
main = do
    opts    <- getOpts
    let input = inputFile opts
    cs      <- readAFP input
    fh      <- openOutputHandle opts
    writeIORef encsRef $ encodings opts
    let put = hPutStr fh
    put "<?xml version=\"1.0\"?>"
    put "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    put "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">"
    put $ htmlPage input
    put "<ol class=\"top\">"
    mapM_ (hPutStrLn fh . (`withChunk` (showHtmlFragment . recHtml . recView))) cs
    put "</ol></body></html>"
    hClose fh

{-# NOINLINE encs #-}
encs :: Encodings
encs = unsafePerformIO (readIORef encsRef)

{-# NOINLINE encsRef #-}
encsRef :: IORef Encodings
encsRef = unsafePerformIO (newIORef (error "oops"))

htmlPage :: String -> String
htmlPage title = showHtmlFragment
    [ header <<
        [ meta !
            [ httpequiv "Content-Type"
            , content "text/html; charset=UTF-8"
            ]
        , thetitle << ("AFP Dump - " ++ title)
        , style !
            [thetype "text/css"
            ] << styles
        ]
    , h1 << title
    ]

styles :: String
styles = joinList "\n" [
    "body { background: #e0e0e0; font-family: times new roman, times; margin-left: 20px }",
    "h1 { font-family: times new roman, times }",
    "span { font-family: andale mono, courier }",
    "ol { border-left: 1px dotted black }",
    "ol.top { border-left: none }",
    "table { font-size: small; border: 0px; border-left: 1px dotted black; padding-left: 6pt; width: 100% }",
    "td.label { background: #d0d0d0; font-family: arial unicode ms, helvetica }",
    "td.item { background: white; width: 100%; font-family: arial unicode ms, helvetica }",
    "div { text-decoration: underline; background: #e0e0ff; font-family: arial unicode ms, helvetica }"
    ]

recHtml :: ViewRecord -> Html
recHtml (ViewRecord t fs)
    | t == typeOf _PTX_TRN
    , (_ : ViewField _ (ViewNStr _ nstr) : []) <- fs
    = li << (typeHtml t +++ ptxHtml (map N1 (S.unpack nstr)))
    | otherwise
    = li << (typeHtml t +++ fieldsHtml fs)

{-# NOINLINE _TypeHtmlCache #-}
_TypeHtmlCache :: H.HashTable RecordType Html 
_TypeHtmlCache = unsafePerformIO $ H.new (==) (hashInt . typeInt)

typeHtml :: RecordType -> Html
typeHtml t = unsafePerformIO $ do
    rv <- H.lookup _TypeHtmlCache t
    case rv of 
        Just html   -> return html
        _           -> do
            let html = typeHtml' t
            H.insert _TypeHtmlCache t html
            return html

typeHtml' :: RecordType -> Html
typeHtml' t = thediv << (typeStr +++ primHtml " &mdash; " +++ typeDesc)
    where
    typeStr = bold << last (split "." typeRepr)
    typeDesc = stringToHtml $ descLookup (MkChunkType $ typeInt t)
    typeRepr = show t

ptxHtml :: [N1] -> [Html]
ptxHtml nstr = [table << textHtml]
    where
    textHtml = textLine ++ [ nstrLine ]
    textLine = [ fieldHtml (ViewField (C.pack $ "(" ++ n ++ ")") (ViewString (typeOf ()) (C.pack txt))) | (n, txt) <- texts nstr ]
    nstrLine = tr << td ! [colspan 2] << thespan << nstrHtml nstr

texts nstr = maybeToList $ msum [ maybe Nothing (Just . ((,) cp)) $ conv (codeName cp) | cp <- encs ]
    where
    conv c@"ibm-937"
        | (even $ length nstr)  = uconv c "utf8" (0x0E : nstr)
        | otherwise             = Nothing
    conv c = uconv c "utf8" nstr
    codeName c
        | isJust $ find (not . isDigit) c   = c
        | otherwise                         = "ibm-" ++ c

fieldsHtml :: [ViewField] -> [Html]
fieldsHtml fs = [table << fsHtml] ++ membersHtml
    where
    fsHtml = [ map fieldHtml fields ]
    membersHtml = chunksHtml $ csHtml ++ dataHtml
    csHtml = [ c | ViewField _ (ViewChunks t c) <- fs ]
    dataHtml = [ c | ViewField _ (ViewData t c) <- fs ]
    fields = sortBy fieldOrder [ v | v@(ViewField str _) <- fs, strOk str ]
    fieldOrder (ViewField a _) (ViewField b _)
        | S.null a  = GT
        | S.null b  = LT
        | otherwise = compare a b
    strOk str
        | S.null str        = True
        | '_' <- C.head str = False
        | otherwise         = Set.notMember str blobFields


blobFields :: Set.Set FieldLabel
blobFields = Set.fromList $ map C.pack
    [ "Data", "EscapeSequence", "Chunks", "ControlCode", "CC", "FlagByte", "Type", "SubType" ]

chunksHtml :: [[ViewRecord]] -> [Html]
chunksHtml [] = []
chunksHtml (cs:_) = [olist << map recHtml cs]

fieldHtml (ViewField str content)
    | S.null str = case content of
        ViewNStr _ nstr | S.null nstr -> noHtml
        _             -> tr << td ! [colspan 2, theclass "item"] << contentHtml content
    | otherwise = tr << [td ! [theclass "label"] << C.unpack str, td ! [theclass "item"] << contentHtml content ]

contentHtml :: ViewContent -> Html
contentHtml x = case x of
    ViewNumber _ n -> stringToHtml $ show n
    ViewString _ s -> stringToHtml $ ['"'] ++ C.unpack s ++ ['"']
    ViewNStr  _ cs -> thespan << nstrHtml (map N1 (S.unpack cs))
    _              -> error (show x)

nstrHtml :: [N1] -> String
nstrHtml nstr
    | length nstr >= 80 = nstrStr nstr ++ "..."
    | otherwise         = nstrStr nstr
    where
    nstrStr :: [N1] -> String
    nstrStr = concatMap ((' ':) . show)
