{-# OPTIONS -fglasgow-exts -funbox-strict-fields -cpp #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Prelude.Exts
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- PreludeExts from the Haskell Wiki under the BSD license.
--
-----------------------------------------------------------------------------

module OpenAFP.Prelude.Exts where
import OpenAFP.Internals

-- | Applies a bunch of functions to a value.
rmap :: [a -> b] -> a -> [b]
rmap fs x = map ($ x) fs

-- Catch user errors, but let others through
onUserError :: IO a -> (String -> IO a) -> IO a
m `onUserError` h
    = m `catchError` \e ->
          if isUserError e
              then h (ioeGetErrorString e)
              else throwError e

type RE = String
(=~) :: String -> String -> Bool
s =~ p = isJust $ matchRegex (mkRegexWithOpts p False True) s

splitRegexWithMatches :: RE -> String -> ([String], [String])
splitRegexWithMatches regexpr src = splitRegex' src
  where
    regex = mkRegex regexpr
    splitRegex' :: String -> ([String], [String])
    splitRegex' s  =
        case matchRegexAll regex s of
             Just (before, match, after, _) ->
                 case splitRegex' after of (text, matches) -> (before : text, match : matches)
             Nothing                    -> ([s], [])

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list =  first : splitBy f (dropWhile f rest)
   where 
     (first, rest) = break f list

lower :: String -> String
lower = map toLower

upper :: String -> String
upper = map toUpper

---------------------------------------------------------------------------------
-- Tom Moertel - from lambdabot's Util module

-- | Join lists with the given glue elements. Example:
--
-- > joinList ", " ["one","two","three"] ===> "one, two, three"

joinList :: [a]   -- ^ Glue to join with
         -> [[a]] -- ^ Elements to glue together
         -> [a]   -- ^ Result: glued-together list

joinList glue xs = (concat . intersperse glue) xs

-- | Split a list into pieces that were held together by glue.  Example:
--
-- > split ", " "one, two, three" ===> ["one","two","three"]

split :: Eq a => [a] -- ^ Glue that holds pieces together
      -> [a]         -- ^ List to break into pieces
      -> [[a]]       -- ^ Result: list of pieces

split glue xs = split' xs
    where
    split' [] = []
    split' xs = piece : split' (dropGlue rest)
        where (piece, rest) = breakOnGlue glue xs
    dropGlue = drop (length glue)

-- | Break off the first piece of a list held together by glue,
--   leaving the glue attached to the remainder of the list.  Example:
--
-- > breakOnGlue ", " "one, two, three" ===> ("one", ", two, three")

breakOnGlue :: (Eq a) => [a] -- ^ Glue that holds pieces together
            -> [a]           -- ^ List from which to break off a piece
            -> ([a],[a])     -- ^ Result: (first piece, glue ++ rest of list)

breakOnGlue _ [] = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], rest)
    | otherwise = (x:piece, rest') where (piece, rest') = breakOnGlue glue xs

split_first_word :: String -> (String, String)
split_first_word xs = (w, dropWhile isSpace xs')
  where (w, xs') = break isSpace xs

