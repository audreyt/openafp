{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.LDTS where
import OpenAFP.Types
import OpenAFP.Internals

data T_LDTS = T_LDTS {
    t_ldts_Type                      :: !N1
    ,t_ldts                          :: !NStr
} deriving (Show, Typeable)

