{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.OCO where
import OpenAFP.Types
import OpenAFP.Internals

data T_OCO = T_OCO {
    t_oco_Type                       :: !N1
    ,t_oco                           :: !NStr
} deriving (Show, Typeable)

