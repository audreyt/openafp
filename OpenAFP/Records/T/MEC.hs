{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.MEC where
import OpenAFP.Types
import OpenAFP.Internals

data T_MEC = T_MEC {
    t_mec_Type                       :: !N1
    ,t_mec                           :: !NStr
} deriving (Show, Typeable)

