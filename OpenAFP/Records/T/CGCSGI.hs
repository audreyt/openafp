{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}
module OpenAFP.Records.T.CGCSGI where
import OpenAFP.Types
import OpenAFP.Internals

data T_CGCSGI = T_CGCSGI {
    t_cgcsgi_Type                    :: !N1
    ,t_cgcsgi                        :: !NStr
} deriving (Show, Typeable)

