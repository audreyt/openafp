{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.RLI where
import OpenAFP.Types
import OpenAFP.Internals

data T_RLI = T_RLI {
    t_rli_Type                       :: !N1
    ,t_rli_SubType                   :: !N1
    ,t_rli                           :: !N1
} deriving (Show, Typeable)

