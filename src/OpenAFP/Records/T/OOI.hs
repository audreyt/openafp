{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.OOI where
import OpenAFP.Types
import OpenAFP.Internals

data T_OOI = T_OOI {
    t_ooi_Type                       :: !N1
    ,t_ooi                           :: !NStr
} deriving (Show, Typeable)

