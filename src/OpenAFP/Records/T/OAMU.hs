{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.OAMU where
import OpenAFP.Types
import OpenAFP.Internals

data T_OAMU = T_OAMU {
    t_oamu_Type                      :: !N1
    ,t_oamu                          :: !NStr
} deriving (Show, Typeable)

