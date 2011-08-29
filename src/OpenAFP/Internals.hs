-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Internals
-- Copyright   :  (c) Audrey Tang 2004-2011
-- License     :  PublicDomain
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
    module X,

    IOm, StateIO, BS,

    hashNew, hashLookup, hashInsert, hashDelete,
    stateGet, statePut
) where
import OpenAFP.Internals.Binary  as X 
import OpenAFP.Internals.Ebc2Asc as X 

import Control.Exception         as X hiding (try, catch, ioError)
import Control.Monad             as X
import Control.Monad.Trans       as X
import Control.Monad.Error       as X (throwError, catchError, MonadError(..), Error(..))
import Control.Monad.Writer      as X
import Control.Monad.Reader      as X
import Control.Monad.RWS         as X hiding (get, put)
import Data.Array.Unboxed        as X 
import Data.Bits                 as X 
import Data.Char                 as X 
import Data.List                 as X 
import Data.HashTable            as X hiding (lookup, insert, delete, new)
import Data.Word                 as X 
import Data.Typeable             as X 
import Data.IORef                as X 
import Data.Maybe                as X 
import Data.Monoid               as X 
import Debug.Trace               as X 
import Foreign.C.String          as X 
import Foreign.C.Types           as X 
import Foreign.Marshal.Alloc     as X 
import Foreign.Marshal.Array     as X 
import Foreign.Marshal.Utils     as X 
import Foreign.Ptr               as X 
import Foreign.ForeignPtr        as X 
import Foreign.Storable          as X 
import Numeric                   as X 
import System.Cmd                as X 
import System.Console.GetOpt     as X 
import System.IO                 as X 
import System.Environment        as X 
import System.Exit               as X 
import System.Mem.Weak           as X 
import System.IO.Unsafe          as X 
import System.IO.Error           as X 
import System.Directory          as X 
import Text.Regex                as X 
import GHC.IOBase                as X (IOArray, newIOArray, readIOArray, writeIOArray)
import qualified Control.Monad.RWS (get, put)
import qualified Control.Monad.State (MonadState)
import qualified Data.HashTable (lookup, insert, delete, new)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

hashNew     = Data.HashTable.new
hashLookup  = Data.HashTable.lookup
hashInsert  = Data.HashTable.insert
hashDelete  = Data.HashTable.delete

type BS = S.ByteString
type BL = L.ByteString

stateGet :: (Control.Monad.State.MonadState s m) => m s
stateGet = Control.Monad.RWS.get
statePut :: (Control.Monad.State.MonadState s m) => s -> m ()
statePut = Control.Monad.RWS.put

type IOm a = (MonadPlus m, MonadIO m, MonadError e m, Show e, Typeable e) => m a
type StateIO v a = (MonadPlus m, MonadIO m, MonadReader v m, MonadError e m, Show e, Typeable e) => m a
