{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.TS where
import OpenAFP.Types
import OpenAFP.Internals

data T_TS = T_TS {
    t_ts_Type                        :: !N1
    ,t_ts                            :: !NStr
} deriving (Show, Typeable)

