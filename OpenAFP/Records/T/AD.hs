{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.AD where
import OpenAFP.Types
import OpenAFP.Internals

data T_AD = T_AD {
    t_ad_Type                        :: !N1
    ,t_ad                            :: !NStr
} deriving (Show, Typeable)

