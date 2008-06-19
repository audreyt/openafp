
module OpenAFP.Prelude.InstanceAFP.M () where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals
import qualified Data.ByteString.Char8 as C

instance Rec MBC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MBC a01 a02 a03
    recPut r = do put $ mbc_Type r; put $ mbc_ r; put $ mbc r; return ()
    recSizeOf r = sum [ sizeOf $ mbc_Type r, sizeOf $ mbc_ r, sizeOf $ mbc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mbc_Type r), viewField "_" (viewNumber $ mbc_ r), viewField "" (viewNStr $ mbc r) ]
    recType = fromEnum . mbc_Type

instance Rec MCA where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MCA a01 a02 a03
    recPut r = do put $ mca_Type r; put $ mca_ r; put $ mca r; return ()
    recSizeOf r = sum [ sizeOf $ mca_Type r, sizeOf $ mca_ r, sizeOf $ mca r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mca_Type r), viewField "_" (viewNumber $ mca_ r), viewField "" (viewNStr $ mca r) ]
    recType = fromEnum . mca_Type

instance Rec MCC where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MCC a01 a02 a03
    recPut r = do put $ mcc_Type r; put $ mcc_ r; put $ mcc r; return ()
    recSizeOf r = sum [ sizeOf $ mcc_Type r, sizeOf $ mcc_ r, sizeOf $ mcc r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mcc_Type r), viewField "_" (viewNumber $ mcc_ r), viewField "" (viewNStr $ mcc r) ]
    recType = fromEnum . mcc_Type

instance Rec MCD where
    recGet = do a01 <- get; a02 <- get; a03 <- get; return $ MCD a01 a02 a03
    recPut r = do put $ mcd_Type r; put $ mcd_ r; put $ mcd r; return ()
    recSizeOf r = sum [ sizeOf $ mcd_Type r, sizeOf $ mcd_ r, sizeOf $ mcd r ]
    recView r = viewRecord (typeOf r) [ viewField "Type" (viewNumber $ mcd_Type r), viewField "_" (viewNumber $ mcd_ r), viewField "" (viewNStr $ mcd r) ]
    recType = fromEnum . mcd_Type

