{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.OBE where
import OpenAFP.Types
import OpenAFP.Internals

data T_OBE = T_OBE {
    t_obe_Type                       :: !N1
    ,t_obe                           :: !NStr
} deriving (Show, Typeable)

