{-# OPTIONS -fglasgow-exts #-}

module OpenAFP.Prelude.InstanceAFP.G () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

instance Rec GAD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ GAD a01 a02 a03
    recPut r = do put $ gad_Type r; put $ gad_ r; put $ gad r; return ()
    recSizeOf r = sum [ sizeOf $ gad_Type r, sizeOf $ gad_ r, sizeOf $ gad r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ gad_Type r), viewField "_" (viewNumber $ gad_ r), viewField "" (viewNStr $ gad r) ]
    recType = fromEnum . gad_Type

instance Rec GDD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ GDD a01 a02 a03
    recPut r = do put $ gdd_Type r; put $ gdd_ r; put $ gdd r; return ()
    recSizeOf r = sum [ sizeOf $ gdd_Type r, sizeOf $ gdd_ r, sizeOf $ gdd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ gdd_Type r), viewField "_" (viewNumber $ gdd_ r), viewField "" (viewNStr $ gdd r) ]
    recType = fromEnum . gdd_Type

