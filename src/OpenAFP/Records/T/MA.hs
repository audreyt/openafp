{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.MA where
import OpenAFP.Types
import OpenAFP.Internals

data T_MA = T_MA {
    t_ma_Type                        :: !N1
    ,t_ma                            :: !NStr
} deriving (Show, Typeable)

