-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Types.Record
-- Copyright   :  (c) Audrey Tang 2004-2011
-- License     :  PublicDomain
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module handles parsed AFP records with named fields.
--
-----------------------------------------------------------------------------

module OpenAFP.Types.Record where
import OpenAFP.Internals
import OpenAFP.Types.View

newtype Record a = Record { fromRecord :: a }
    deriving (Show, Eq, Typeable)

class (Show a, Typeable a) => Rec a where
    recGet :: Get a
    recGet = error "recGet not defined"
    recPut :: a -> Put
    recPut x = error ("recPut not defined: " ++ show x)
    recSizeOf :: a -> Int
    recSizeOf x = error ("recSizeOf not defined: " ++ show x)
    recView :: a -> ViewRecord
    recView x = error ("recView not defined: " ++ show x)
    recType :: a -> Int
    recType x = error ("recType not defined: " ++ show x)

instance Rec a => Storable (Record a) where
    alignment (Record r) = 8
    sizeOf (Record r) = recSizeOf r

instance Rec a => Rec (Record a) where
    recGet               = liftM Record recGet
    recPut (Record a)    = recPut a
    recView (Record a)   = recView a
    recSizeOf (Record a) = recSizeOf a
    recType (Record a)   = recType a

instance Rec a => Binary (Record a) where
    get = fmap Record recGet
    put (Record a) = recPut a

instance (Binary a, Rec a, Show a, Typeable a) => Rec [a] where
    recSizeOf list = sum $ map recSizeOf list
    recGet = getList
    recPut = mapM_ put

instance Rec N1 where
    recSizeOf _ = 1

