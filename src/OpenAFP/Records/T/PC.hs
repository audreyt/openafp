module OpenAFP.Records.T.PC where
import OpenAFP.Types
import OpenAFP.Internals

data T_PC = T_PC {
    t_pc_Type                        :: !N1
    ,t_pc                            :: !NStr
} deriving (Show, Typeable)

