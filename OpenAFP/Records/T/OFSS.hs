{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.OFSS where
import OpenAFP.Types
import OpenAFP.Internals

data T_OFSS = T_OFSS {
    t_ofss_Type                      :: !N1
    ,t_ofss                          :: !NStr
} deriving (Show, Typeable)

