{-# OPTIONS -fglasgow-exts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Internals
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module imports and re-exports external modules used by OpenAFP,
-- as well as OpenAFP's own internal modules.
--
-----------------------------------------------------------------------------

module OpenAFP.Internals (
    module OpenAFP.Internals.Binary,
    module OpenAFP.Internals.Ebc2Asc,

    module Control.Exception,
    module Control.Monad.RWS,
    module Control.Monad.Error,
    module Data.Array.Unboxed,
    module Data.Bits,
    module Data.Char,
    module Data.List,
    module Data.HashTable,
    module Data.Word,
    module Data.Typeable,
    module Data.IORef,
    module Data.Maybe,
    module Debug.Trace,
    module Foreign.C.String,
    module Foreign.C.Types,
    module Foreign.Marshal.Alloc,
    module Foreign.Marshal.Array,
    module Foreign.Marshal.Utils,
    module Foreign.Ptr,
    module Foreign.ForeignPtr,
    module Foreign.Storable,
    module System.Mem.Weak,
    module System.Console.GetOpt,
    module System.IO,
    module System.Cmd,
    module System.Environment,
    module System.Exit,
    module System.IO.Unsafe,
    module System.IO.Error,
    module System.Directory,
    module Text.Regex,
    module GHC.IOBase,

    IOm, StateIO,

    hashNew, hashLookup, hashInsert, hashDelete,
    stateGet, statePut
) where
import OpenAFP.Internals.Binary
import OpenAFP.Internals.Ebc2Asc

import Control.Exception hiding (try, catch, ioError)
import Control.Monad.Error (throwError, catchError, MonadError(..), Error(..))
import Control.Monad.RWS hiding (get, put)
import qualified Control.Monad.RWS (get, put)
import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.List
import Data.HashTable hiding (lookup, insert, delete, new)
import qualified Data.HashTable (lookup, insert, delete, new)
import Data.Word
import Data.Typeable
import Data.IORef
import Data.Maybe
import Debug.Trace
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Numeric
import System.Cmd
import System.Console.GetOpt
import System.IO
import System.Environment
import System.Exit
import System.Mem.Weak
import System.IO.Unsafe
import System.IO.Error
import System.Directory
import Text.Regex
import GHC.IOBase (IOArray, newIOArray, readIOArray, writeIOArray)

hashNew     = Data.HashTable.new
hashLookup  = Data.HashTable.lookup
hashInsert  = Data.HashTable.insert
hashDelete  = Data.HashTable.delete

stateGet :: (Control.Monad.RWS.MonadState s m) => m s
stateGet = Control.Monad.RWS.get
statePut :: (Control.Monad.RWS.MonadState s m) => s -> m ()
statePut = Control.Monad.RWS.put

type IOm a = (MonadPlus m, MonadIO m, MonadError e m, Show e, Typeable e) => m a
type StateIO v a = (MonadPlus m, MonadIO m, MonadReader v m, MonadError e m, Show e, Typeable e) => m a
