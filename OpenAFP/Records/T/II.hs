{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.II where
import OpenAFP.Types
import OpenAFP.Internals

data T_II = T_II {
    t_ii_Type                        :: !N1
    ,t_ii                            :: !NStr
} deriving (Show, Typeable)

