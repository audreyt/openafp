{-# OPTIONS -O -fglasgow-exts -funbox-strict-fields -fimplicit-params #-}

module Main where
import Text.Html
import OpenAFP.Prelude hiding ((!))
import OpenAFP.Internals.UConv

-- The key here is inventing a ConcreteDataView for our data structure.
-- See OpenAFP.Types.View for details.

type Encodings = [String]
type WithVars a = (?encs :: Encodings) => a

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
    let put = (hPutStr fh)
    put $ renderMessage ++ "<HTML>" ++ htmlPage input
    put $ "<OL class='top'>"
    let ?encs = (encodings opts)
    mapM_ (`withChunk` viewRec put) cs
    put $ "</OL></HTML>"
    hClose fh

viewRec :: WithVars ((Rec a) => (String -> IO ()) -> a -> IO ())
viewRec put r = do
    rec <- recView r
    mapM_ put [ dropWhile isSpace l | l <- lines . show $ recHtml rec ]

htmlPage :: String -> String
htmlPage title = show
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

recHtml :: WithVars (ViewRecord -> Html)
recHtml (ViewRecord (t, fs))
    | t == typeOf _PTX_TRN
    , (_ : ViewField (_, ViewNStr (_, nstr)) : []) <- fs
    = li << (typeHtml t +++ ptxHtml nstr)
    | otherwise
    = li << (typeHtml t +++ fieldsHtml fs)

typeHtml :: ChunkType -> Html
typeHtml t = thediv << (typeStr +++ primHtml " &mdash; " +++ typeDesc)
    where
    typeStr = bold << last (split "." (show t))
    typeDesc = stringToHtml $ descLookup t

ptxHtml :: WithVars ([N1] -> [Html])
ptxHtml nstr = [table << textHtml]
    where
    textHtml = textLine ++ [ nstrLine ]
    textLine = [ fieldHtml ("(" ++ n ++ ")", ViewString (undefined, txt)) | (n, txt) <- texts nstr ]
    nstrLine = tr << td ! [colspan 2] << thespan << nstrHtml nstr

texts :: WithVars ([N1] -> [(String, String)])
texts nstr = maybeToList $ msum [ maybe Nothing (Just . ((,) cp)) $ conv (codeName cp) | cp <- ?encs ]
    where
    conv c@"ibm-937"
        | (even $ length nstr)  = uconv c "utf8" (0x0E : nstr)
        | otherwise             = Nothing
    conv c = uconv c "utf8" nstr
    codeName c
        | isJust $ find (not . isDigit) c   = c
        | otherwise                         = "ibm-" ++ c

fieldsHtml :: WithVars ([ViewField] -> [Html])
fieldsHtml fs = [table << fsHtml] ++ membersHtml
    where
    fsHtml = [ map fieldHtml fields ]
    membersHtml = chunksHtml $ csHtml ++ dataHtml
    csHtml = [ c | ViewField (_, ViewChunks (t, c)) <- fs ]
    dataHtml = [ c | ViewField (_, ViewData (t, c)) <- fs ]
    fields = sortBy fieldOrder [ field | ViewField field@(str, _) <- fs, strOk str ]
    fieldOrder ("", _) _    = GT
    fieldOrder _ ("", _)    = LT
    fieldOrder (a,_) (b,_)  = compare a b
    strOk ('_':_)           = False
    strOk "Data"            = False
    strOk "EscapeSequence"  = False
    strOk "Chunks"          = False
    strOk "ControlCode"     = False
    strOk "CC"              = False
    strOk "FlagByte"        = False
    strOk "Type"            = False
    strOk "SubType"         = False
    strOk _                 = True

chunksHtml :: WithVars ([[ViewRecord]] -> [Html])
chunksHtml [] = []
chunksHtml (cs:_) = [olist << map recHtml cs]

fieldHtml :: (String, ViewContent) -> Html
fieldHtml ("", ViewNStr (_, [])) = noHtml
fieldHtml ("", content) =
    tr << td ! [colspan 2, theclass "item"] << contentHtml content
fieldHtml (str, content) =
    tr << [td ! [theclass "label"] << str, td ! [theclass "item"] << contentHtml content ]

contentHtml :: ViewContent -> Html
contentHtml x = case x of
    ViewNumber (_, n) -> stringToHtml $ show n
    ViewString (_, s) -> stringToHtml $ ['"'] ++ s ++ ['"']
    ViewNStr  (_, cs) -> thespan << nstrHtml cs
    _                 -> error (show x)

nstrHtml :: [N1] -> String
nstrHtml nstr
    | length nstr >= 80 = nstrStr nstr ++ "..."
    | otherwise         = nstrStr nstr
    where
    nstrStr :: [N1] -> String
    nstrStr = concatMap ((' ':) . show)
