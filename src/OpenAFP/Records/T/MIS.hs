{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.MIS where
import OpenAFP.Types
import OpenAFP.Internals

data T_MIS = T_MIS {
    t_mis_Type                       :: !N1
    ,t_mis                           :: !NStr
} deriving (Show, Typeable)

