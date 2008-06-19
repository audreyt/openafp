module OpenAFP.Records.T.ROI where
import OpenAFP.Types
import OpenAFP.Internals

data T_ROI = T_ROI {
    t_roi_Type                       :: !N1
    ,t_roi                           :: !NStr
} deriving (Show, Typeable)

