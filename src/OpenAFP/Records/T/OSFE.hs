{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.OSFE where
import OpenAFP.Types
import OpenAFP.Internals

data T_OSFE = T_OSFE {
    t_osfe_Type                      :: !N1
    ,t_osfe                          :: !NStr
} deriving (Show, Typeable)

