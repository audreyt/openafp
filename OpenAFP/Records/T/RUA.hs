{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.RUA where
import OpenAFP.Types
import OpenAFP.Internals

data T_RUA = T_RUA {
    t_rua_Type                       :: !N1
    ,t_rua                           :: !NStr
} deriving (Show, Typeable)

