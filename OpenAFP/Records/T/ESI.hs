{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.ESI where
import OpenAFP.Types
import OpenAFP.Internals

data T_ESI = T_ESI {
    t_esi_Type                       :: !N1
    ,t_esi                           :: !NStr
} deriving (Show, Typeable)

