module OpenAFP.Records.T (
    module OpenAFP.Records.T.AD, _T_AD,
    module OpenAFP.Records.T.AQ, _T_AQ,
    module OpenAFP.Records.T.AV, _T_AV,
    module OpenAFP.Records.T.C, _T_C,
    module OpenAFP.Records.T.CF, _T_CF,
    module OpenAFP.Records.T.CGCSGI, _T_CGCSGI,
    module OpenAFP.Records.T.CR, _T_CR,
    module OpenAFP.Records.T.CS, _T_CS,
    module OpenAFP.Records.T.DP, _T_DP,
    module OpenAFP.Records.T.EF, _T_EF,
    module OpenAFP.Records.T.ERLI, _T_ERLI,
    module OpenAFP.Records.T.ESI, _T_ESI,
    module OpenAFP.Records.T.FCGCSGI, _T_FCGCSGI,
    module OpenAFP.Records.T.FDS, _T_FDS,
    module OpenAFP.Records.T.FF, _T_FF,
    module OpenAFP.Records.T.FHSF, _T_FHSF,
    module OpenAFP.Records.T.FO, _T_FO,
    module OpenAFP.Records.T.FQN, _T_FQN,
    module OpenAFP.Records.T.FRMT, _T_FRMT,
    module OpenAFP.Records.T.II, _T_II,
    module OpenAFP.Records.T.LDOPM, _T_LDOPM,
    module OpenAFP.Records.T.LDTS, _T_LDTS,
    module OpenAFP.Records.T.MA, _T_MA,
    module OpenAFP.Records.T.MEC, _T_MEC,
    module OpenAFP.Records.T.MF, _T_MF,
    module OpenAFP.Records.T.MIS, _T_MIS,
    module OpenAFP.Records.T.MMPN, _T_MMPN,
    module OpenAFP.Records.T.MO, _T_MO,
    module OpenAFP.Records.T.MOR, _T_MOR,
    module OpenAFP.Records.T.OAMU, _T_OAMU,
    module OpenAFP.Records.T.OAS, _T_OAS,
    module OpenAFP.Records.T.OBE, _T_OBE,
    module OpenAFP.Records.T.OBO, _T_OBO,
    module OpenAFP.Records.T.OCH, _T_OCH,
    module OpenAFP.Records.T.OCL, _T_OCL,
    module OpenAFP.Records.T.OCO, _T_OCO,
    module OpenAFP.Records.T.OFSS, _T_OFSS,
    module OpenAFP.Records.T.OO, _T_OO,
    module OpenAFP.Records.T.OOI, _T_OOI,
    module OpenAFP.Records.T.OSFE, _T_OSFE,
    module OpenAFP.Records.T.OSFO, _T_OSFO,
    module OpenAFP.Records.T.PC, _T_PC,
    module OpenAFP.Records.T.POCP, _T_POCP,
    module OpenAFP.Records.T.PPI, _T_PPI,
    module OpenAFP.Records.T.PSMR, _T_PSMR,
    module OpenAFP.Records.T.PSRM, _T_PSRM,
    module OpenAFP.Records.T.PV, _T_PV,
    module OpenAFP.Records.T.RLI, _T_RLI,
    module OpenAFP.Records.T.ROI, _T_ROI,
    module OpenAFP.Records.T.ROT, _T_ROT,
    module OpenAFP.Records.T.RSN, _T_RSN,
    module OpenAFP.Records.T.RUA, _T_RUA,
    module OpenAFP.Records.T.T1CRMT, _T_T1CRMT,
    module OpenAFP.Records.T.T2FRMT, _T_T2FRMT,
    module OpenAFP.Records.T.TO, _T_TO,
    module OpenAFP.Records.T.TS, _T_TS,
    module OpenAFP.Records.T.UDTS, _T_UDTS,
) where
import OpenAFP.Types
import OpenAFP.Records.T.AD
import OpenAFP.Records.T.AQ
import OpenAFP.Records.T.AV
import OpenAFP.Records.T.C
import OpenAFP.Records.T.CF
import OpenAFP.Records.T.CGCSGI
import OpenAFP.Records.T.CR
import OpenAFP.Records.T.CS
import OpenAFP.Records.T.DP
import OpenAFP.Records.T.EF
import OpenAFP.Records.T.ERLI
import OpenAFP.Records.T.ESI
import OpenAFP.Records.T.FCGCSGI
import OpenAFP.Records.T.FDS
import OpenAFP.Records.T.FF
import OpenAFP.Records.T.FHSF
import OpenAFP.Records.T.FO
import OpenAFP.Records.T.FQN
import OpenAFP.Records.T.FRMT
import OpenAFP.Records.T.II
import OpenAFP.Records.T.LDOPM
import OpenAFP.Records.T.LDTS
import OpenAFP.Records.T.MA
import OpenAFP.Records.T.MEC
import OpenAFP.Records.T.MF
import OpenAFP.Records.T.MIS
import OpenAFP.Records.T.MMPN
import OpenAFP.Records.T.MO
import OpenAFP.Records.T.MOR
import OpenAFP.Records.T.OAMU
import OpenAFP.Records.T.OAS
import OpenAFP.Records.T.OBE
import OpenAFP.Records.T.OBO
import OpenAFP.Records.T.OCH
import OpenAFP.Records.T.OCL
import OpenAFP.Records.T.OCO
import OpenAFP.Records.T.OFSS
import OpenAFP.Records.T.OO
import OpenAFP.Records.T.OOI
import OpenAFP.Records.T.OSFE
import OpenAFP.Records.T.OSFO
import OpenAFP.Records.T.PC
import OpenAFP.Records.T.POCP
import OpenAFP.Records.T.PPI
import OpenAFP.Records.T.PSMR
import OpenAFP.Records.T.PSRM
import OpenAFP.Records.T.PV
import OpenAFP.Records.T.RLI
import OpenAFP.Records.T.ROI
import OpenAFP.Records.T.ROT
import OpenAFP.Records.T.RSN
import OpenAFP.Records.T.RUA
import OpenAFP.Records.T.T1CRMT
import OpenAFP.Records.T.T2FRMT
import OpenAFP.Records.T.TO
import OpenAFP.Records.T.TS
import OpenAFP.Records.T.UDTS

_T_AD :: T_AD
_T_AD = T_AD 0x4D _NStr
_T_AQ :: T_AQ
_T_AQ = T_AQ 0x80 _NStr
_T_AV :: T_AV
_T_AV = T_AV 0x36 _NStr
_T_C  :: T_C
_T_C  = T_C  0x65 _NStr
_T_CF :: T_CF
_T_CF = T_CF 0x75 _NStr
_T_CGCSGI :: T_CGCSGI
_T_CGCSGI = T_CGCSGI 0x01 _NStr
_T_CR :: T_CR
_T_CR = T_CR 0x26 _NStr
_T_CS :: T_CS
_T_CS = T_CS 0x4E _NStr
_T_DP :: T_DP
_T_DP = T_DP 0x43 _NStr
_T_EF :: T_EF
_T_EF = T_EF 0x6D _NStr
_T_ERLI :: T_ERLI
_T_ERLI = T_ERLI 0x23 _NStr
_T_ESI :: T_ESI
_T_ESI = T_ESI 0x50 _NStr
_T_FCGCSGI :: T_FCGCSGI
_T_FCGCSGI = T_FCGCSGI 0x20 _NStr
_T_FDS :: T_FDS
_T_FDS = T_FDS 0x1F _NStr
_T_FF :: T_FF
_T_FF = T_FF 0x78 _NStr
_T_FHSF :: T_FHSF
_T_FHSF = T_FHSF 0x5D _NStr
_T_FO :: T_FO
_T_FO = T_FO 0x85 _NStr
_T_FQN :: T_FQN
_T_FQN = T_FQN 0x02 0 0 _NStr
_T_FRMT :: T_FRMT
_T_FRMT = T_FRMT 0x84 _NStr
_T_II :: T_II
_T_II = T_II 0x73 _NStr
_T_LDOPM :: T_LDOPM
_T_LDOPM = T_LDOPM 0x27 _NStr
_T_LDTS :: T_LDTS
_T_LDTS = T_LDTS 0x62 _NStr
_T_MA :: T_MA
_T_MA = T_MA 0x79 _NStr
_T_MEC :: T_MEC
_T_MEC = T_MEC 0x45 _NStr
_T_MF :: T_MF
_T_MF = T_MF 0x87 _NStr
_T_MIS :: T_MIS
_T_MIS = T_MIS 0x18 _NStr
_T_MMPN :: T_MMPN
_T_MMPN = T_MMPN 0x56 _NStr
_T_MO :: T_MO
_T_MO = T_MO 0x04 _NStr
_T_MOR :: T_MOR
_T_MOR = T_MOR 0x68 _NStr
_T_OAMU :: T_OAMU
_T_OAMU = T_OAMU 0x4B _NStr
_T_OAS :: T_OAS
_T_OAS = T_OAS 0x4C _NStr
_T_OBE :: T_OBE
_T_OBE = T_OBE 0x57 _NStr
_T_OBO :: T_OBO
_T_OBO = T_OBO 0x2D _NStr
_T_OCH :: T_OCH
_T_OCH = T_OCH 0x63 _NStr
_T_OCL :: T_OCL
_T_OCL = T_OCL 0x10 _NStr
_T_OCO :: T_OCO
_T_OCO = T_OCO 0x5E _NStr
_T_OFSS :: T_OFSS
_T_OFSS = T_OFSS 0x21 _NStr
_T_OO :: T_OO
_T_OO = T_OO 0x5A _NStr
_T_OOI :: T_OOI
_T_OOI = T_OOI 0x64 _NStr
_T_OSFE :: T_OSFE
_T_OSFE = T_OSFE 0x59 _NStr
_T_OSFO :: T_OSFO
_T_OSFO = T_OSFO 0x58 _NStr
_T_PC :: T_PC
_T_PC = T_PC 0x83 _NStr
_T_POCP :: T_POCP
_T_POCP = T_POCP 0x46 _NStr
_T_PPI :: T_PPI
_T_PPI = T_PPI 0x81 _NStr
_T_PSMR :: T_PSMR
_T_PSMR = T_PSMR 0x71 _NStr
_T_PSRM :: T_PSRM
_T_PSRM = T_PSRM 0x70 _NStr
_T_PV :: T_PV
_T_PV = T_PV 0x82 _NStr
_T_RLI :: T_RLI
_T_RLI = T_RLI 0x24 0 0
_T_ROI :: T_ROI
_T_ROI = T_ROI 0x6C _NStr
_T_ROT :: T_ROT
_T_ROT = T_ROT 0x22 _NStr
_T_RSN :: T_RSN
_T_RSN = T_RSN 0x25 _NStr
_T_RUA :: T_RUA
_T_RUA = T_RUA 0x47 _NStr
_T_T1CRMT :: T_T1CRMT
_T_T1CRMT = T_T1CRMT 0x63 _NStr
_T_T2FRMT :: T_T2FRMT
_T_T2FRMT = T_T2FRMT 0x63 _NStr
_T_TO :: T_TO
_T_TO = T_TO 0x1D _NStr
_T_TS :: T_TS
_T_TS = T_TS 0x74 _NStr
_T_UDTS :: T_UDTS
_T_UDTS = T_UDTS 0x72 _NStr

