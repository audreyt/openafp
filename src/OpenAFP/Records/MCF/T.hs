{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.MCF.T where
import OpenAFP.Types
import OpenAFP.Internals

data MCF_T = MCF_T {
    mcf_t_Chunks                  :: ![T_]
} deriving (Show, Typeable)

