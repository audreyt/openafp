{-# OPTIONS -O -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types.Record
-- Copyright   :  (c) Autrijus Tang 2004
-- License     :  BSD-style
-- 
-- Maintainer  :  autrijus@autrijus.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module handles parsed AFP records with named fields.
--
-----------------------------------------------------------------------------

module OpenAFP.Types.Record where
import OpenAFP.Internals
import OpenAFP.Types.View

newtype Record a = Record a
    deriving (Show, Eq, Typeable)

class (Show a, Typeable a) => Rec a where
    recGet :: BinHandle -> IO a
    recGet = error "recGet not defined"
    recPut :: BinHandle -> a -> IO ()
    recPut = error "recPut not defined"
    recSizeOf :: a -> Int
    recSizeOf = error "recSizeOf not defined"
    recView :: a -> IO ViewRecord
    recView = error "recView not defined"
    recType :: a -> Int
    recType = error "recType not defined"

-- (forall a. (Rec a) => (a -> m b))
instance (Rec a) => Storable (Record a) where
    alignment (Record r) = 8
    sizeOf (Record r) = recSizeOf r
    -- sizeOf (Record r) = sum $ recApply func r

{-
func :: (Storable x) => String -> x -> Int
func x y = sizeOf y
-}
func x y = 0

instance (Rec a) => Rec (Record a)

instance (Rec a) => Binary (Record a) where
    get bh = do
        b <- recGet bh
        return $ Record b
    put bh (Record a) = do
        recPut bh a
        return ()

instance (Binary a, Rec a, Show a, Typeable a) => Rec [a] where
    recSizeOf list = sum $ map recSizeOf list
    recGet bh = get bh
    recPut bh list = do
        mapM_ (put bh) list

instance Rec N1 where
    recSizeOf _ = 1

fromRecord (Record a) = a

