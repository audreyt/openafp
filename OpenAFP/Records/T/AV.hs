{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.AV where
import OpenAFP.Types
import OpenAFP.Internals

data T_AV = T_AV {
    t_av_Type                        :: !N1
    ,t_av                            :: !NStr
} deriving (Show, Typeable)

