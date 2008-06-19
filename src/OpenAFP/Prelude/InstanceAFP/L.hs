
module OpenAFP.Prelude.InstanceAFP.L () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals

instance Rec LLE where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ LLE a01 a02 a03
    recPut r = do put $ lle_Type r; put $ lle_ r; put $ lle r; return ()
    recSizeOf r = sum [ sizeOf $ lle_Type r, sizeOf $ lle_ r, sizeOf $ lle r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ lle_Type r), viewField "_" (viewNumber $ lle_ r), viewField "" (viewNStr $ lle r) ]
    recType = fromEnum . lle_Type

instance Rec LNC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ LNC a01 a02 a03
    recPut r = do put $ lnc_Type r; put $ lnc_ r; put $ lnc r; return ()
    recSizeOf r = sum [ sizeOf $ lnc_Type r, sizeOf $ lnc_ r, sizeOf $ lnc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ lnc_Type r), viewField "_" (viewNumber $ lnc_ r), viewField "" (viewNStr $ lnc r) ]
    recType = fromEnum . lnc_Type

instance Rec LND where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ LND a01 a02 a03
    recPut r = do put $ lnd_Type r; put $ lnd_ r; put $ lnd r; return ()
    recSizeOf r = sum [ sizeOf $ lnd_Type r, sizeOf $ lnd_ r, sizeOf $ lnd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ lnd_Type r), viewField "_" (viewNumber $ lnd_ r), viewField "" (viewNStr $ lnd r) ]
    recType = fromEnum . lnd_Type

