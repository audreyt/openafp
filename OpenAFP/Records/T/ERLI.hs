{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.ERLI where
import OpenAFP.Types
import OpenAFP.Internals

data T_ERLI = T_ERLI {
    t_erli_Type                      :: !N1
    ,t_erli                          :: !NStr
} deriving (Show, Typeable)

