{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstanceAFP.D () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

instance Rec DXD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ DXD a01 a02 a03
    recPut r = do put $ dxd_Type r; put $ dxd_ r; put $ dxd r; return ()
    recSizeOf r = sum [ sizeOf $ dxd_Type r, sizeOf $ dxd_ r, sizeOf $ dxd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ dxd_Type r), viewField "_" (viewNumber $ dxd_ r), viewField "" (viewNStr $ dxd r) ]
    recType = fromEnum . dxd_Type

