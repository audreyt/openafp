{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.FQN where
import OpenAFP.Types
import OpenAFP.Internals

data T_FQN = T_FQN {
    t_fqn_Type                       :: !N1
    ,t_fqn_SubType                   :: !N1
    ,t_fqn_Format                    :: !N1
    ,t_fqn                           :: !AStr
} deriving (Show, Typeable)

