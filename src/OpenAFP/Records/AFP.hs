module OpenAFP.Records.AFP (
    module OpenAFP.Records.AFP.BAG, _BAG,
    module OpenAFP.Records.AFP.BBC, _BBC,
    module OpenAFP.Records.AFP.BCA, _BCA,
    module OpenAFP.Records.AFP.BCF, _BCF,
    module OpenAFP.Records.AFP.BCP, _BCP,
    module OpenAFP.Records.AFP.BDA, _BDA,
    module OpenAFP.Records.AFP.BDD, _BDD,
    module OpenAFP.Records.AFP.BDG, _BDG,
    module OpenAFP.Records.AFP.BDI, _BDI,
    module OpenAFP.Records.AFP.BDM, _BDM,
    module OpenAFP.Records.AFP.BDT, _BDT,
    module OpenAFP.Records.AFP.BDX, _BDX,
    module OpenAFP.Records.AFP.BFG, _BFG,
    module OpenAFP.Records.AFP.BFM, _BFM,
    module OpenAFP.Records.AFP.BFN, _BFN,
    module OpenAFP.Records.AFP.BGR, _BGR,
    module OpenAFP.Records.AFP.BII, _BII,
    module OpenAFP.Records.AFP.BIM, _BIM,
    module OpenAFP.Records.AFP.BMM, _BMM,
    module OpenAFP.Records.AFP.BMO, _BMO,
    module OpenAFP.Records.AFP.BNG, _BNG,
    module OpenAFP.Records.AFP.BOC, _BOC,
    module OpenAFP.Records.AFP.BOG, _BOG,
    module OpenAFP.Records.AFP.BPG, _BPG,
    module OpenAFP.Records.AFP.BPM, _BPM,
    module OpenAFP.Records.AFP.BPS, _BPS,
    module OpenAFP.Records.AFP.BPT, _BPT,
    module OpenAFP.Records.AFP.BR, _BR,
    module OpenAFP.Records.AFP.BRG, _BRG,
    module OpenAFP.Records.AFP.BSG, _BSG,
    module OpenAFP.Records.AFP.CAT, _CAT,
    module OpenAFP.Records.AFP.CDD, _CDD,
    module OpenAFP.Records.AFP.CFC, _CFC,
    module OpenAFP.Records.AFP.CFI, _CFI,
    _CFI_Data,
    module OpenAFP.Records.AFP.CPC, _CPC,
    module OpenAFP.Records.AFP.CPD, _CPD,
    module OpenAFP.Records.AFP.CPI, _CPI,
    _CPI_Data,
    module OpenAFP.Records.AFP.CTC, _CTC,
    module OpenAFP.Records.AFP.DXD, _DXD,
    module OpenAFP.Records.AFP.EAG, _EAG,
    module OpenAFP.Records.AFP.EBC, _EBC,
    module OpenAFP.Records.AFP.ECA, _ECA,
    module OpenAFP.Records.AFP.ECF, _ECF,
    module OpenAFP.Records.AFP.ECP, _ECP,
    module OpenAFP.Records.AFP.EDG, _EDG,
    module OpenAFP.Records.AFP.EDI, _EDI,
    module OpenAFP.Records.AFP.EDM, _EDM,
    module OpenAFP.Records.AFP.EDT, _EDT,
    module OpenAFP.Records.AFP.EDX, _EDX,
    module OpenAFP.Records.AFP.EFG, _EFG,
    module OpenAFP.Records.AFP.EFM, _EFM,
    module OpenAFP.Records.AFP.EFN, _EFN,
    module OpenAFP.Records.AFP.EGR, _EGR,
    module OpenAFP.Records.AFP.EII, _EII,
    module OpenAFP.Records.AFP.EIM, _EIM,
    module OpenAFP.Records.AFP.EMM, _EMM,
    module OpenAFP.Records.AFP.EMO, _EMO,
    module OpenAFP.Records.AFP.ENG, _ENG,
    module OpenAFP.Records.AFP.EOC, _EOC,
    module OpenAFP.Records.AFP.EOG, _EOG,
    module OpenAFP.Records.AFP.EPG, _EPG,
    module OpenAFP.Records.AFP.EPM, _EPM,
    module OpenAFP.Records.AFP.EPS, _EPS,
    module OpenAFP.Records.AFP.EPT, _EPT,
    module OpenAFP.Records.AFP.ER, _ER,
    module OpenAFP.Records.AFP.ERG, _ERG,
    module OpenAFP.Records.AFP.ESG, _ESG,
    module OpenAFP.Records.AFP.FGD, _FGD,
    module OpenAFP.Records.AFP.FNC, _FNC,
    module OpenAFP.Records.AFP.FND, _FND,
    module OpenAFP.Records.AFP.FNG, _FNG,
    module OpenAFP.Records.AFP.FNI, _FNI,
    _FNI_Data,
    module OpenAFP.Records.AFP.FNM, _FNM,
    _FNM_Data,
    module OpenAFP.Records.AFP.FNN, _FNN,
    module OpenAFP.Records.AFP.FNO, _FNO,
    module OpenAFP.Records.AFP.FNP, _FNP,
    module OpenAFP.Records.AFP.GAD, _GAD,
    module OpenAFP.Records.AFP.GDD, _GDD,
    module OpenAFP.Records.AFP.ICP, _ICP,
    module OpenAFP.Records.AFP.IDD, _IDD,
    module OpenAFP.Records.AFP.IEL, _IEL,
    module OpenAFP.Records.AFP.IID, _IID,
    module OpenAFP.Records.AFP.IMM, _IMM,
    module OpenAFP.Records.AFP.IOB, _IOB,
    module OpenAFP.Records.AFP.IOC, _IOC,
    module OpenAFP.Records.AFP.IPD, _IPD,
    module OpenAFP.Records.AFP.IPG, _IPG,
    module OpenAFP.Records.AFP.IPO, _IPO,
    module OpenAFP.Records.AFP.IPS, _IPS,
    module OpenAFP.Records.AFP.IRD, _IRD,
    module OpenAFP.Records.AFP.LLE, _LLE,
    module OpenAFP.Records.AFP.LNC, _LNC,
    module OpenAFP.Records.AFP.LND, _LND,
    module OpenAFP.Records.AFP.MBC, _MBC,
    module OpenAFP.Records.AFP.MCA, _MCA,
    module OpenAFP.Records.AFP.MCC, _MCC,
    module OpenAFP.Records.AFP.MCD, _MCD,
    module OpenAFP.Records.AFP.MCF, _MCF,
    module OpenAFP.Records.AFP.MCF1, _MCF1,
    _MCF1_Data,
    module OpenAFP.Records.AFP.MDD, _MDD,
    module OpenAFP.Records.AFP.MDR, _MDR,
    module OpenAFP.Records.AFP.MFC, _MFC,
    module OpenAFP.Records.AFP.MGO, _MGO,
    module OpenAFP.Records.AFP.MIO, _MIO,
    module OpenAFP.Records.AFP.MMC, _MMC,
    module OpenAFP.Records.AFP.MMO, _MMO,
    module OpenAFP.Records.AFP.MMT, _MMT,
    module OpenAFP.Records.AFP.MPG, _MPG,
    module OpenAFP.Records.AFP.MPO, _MPO,
    module OpenAFP.Records.AFP.MPS, _MPS,
    module OpenAFP.Records.AFP.MSU, _MSU,
    module OpenAFP.Records.AFP.NOP, _NOP,
    module OpenAFP.Records.AFP.OBD, _OBD,
    module OpenAFP.Records.AFP.OBP, _OBP,
    module OpenAFP.Records.AFP.OCD, _OCD,
    module OpenAFP.Records.AFP.PFC, _PFC,
    module OpenAFP.Records.AFP.PGD, _PGD,
    module OpenAFP.Records.AFP.PGP, _PGP,
    module OpenAFP.Records.AFP.PGP1, _PGP1,
    module OpenAFP.Records.AFP.PMC, _PMC,
    module OpenAFP.Records.AFP.PTD, _PTD,
    module OpenAFP.Records.AFP.PTD1, _PTD1,
    module OpenAFP.Records.AFP.PTX, _PTX,
    module OpenAFP.Records.AFP.TLE, _TLE,
) where
import OpenAFP.Types
import OpenAFP.Records.AFP.BAG
import OpenAFP.Records.AFP.BBC
import OpenAFP.Records.AFP.BCA
import OpenAFP.Records.AFP.BCF
import OpenAFP.Records.AFP.BCP
import OpenAFP.Records.AFP.BDA
import OpenAFP.Records.AFP.BDD
import OpenAFP.Records.AFP.BDG
import OpenAFP.Records.AFP.BDI
import OpenAFP.Records.AFP.BDM
import OpenAFP.Records.AFP.BDT
import OpenAFP.Records.AFP.BDX
import OpenAFP.Records.AFP.BFG
import OpenAFP.Records.AFP.BFM
import OpenAFP.Records.AFP.BFN
import OpenAFP.Records.AFP.BGR
import OpenAFP.Records.AFP.BII
import OpenAFP.Records.AFP.BIM
import OpenAFP.Records.AFP.BMM
import OpenAFP.Records.AFP.BMO
import OpenAFP.Records.AFP.BNG
import OpenAFP.Records.AFP.BOC
import OpenAFP.Records.AFP.BOG
import OpenAFP.Records.AFP.BPG
import OpenAFP.Records.AFP.BPM
import OpenAFP.Records.AFP.BPS
import OpenAFP.Records.AFP.BPT
import OpenAFP.Records.AFP.BR
import OpenAFP.Records.AFP.BRG
import OpenAFP.Records.AFP.BSG
import OpenAFP.Records.AFP.CAT
import OpenAFP.Records.AFP.CDD
import OpenAFP.Records.AFP.CFC
import OpenAFP.Records.AFP.CFI
import OpenAFP.Records.AFP.CPC
import OpenAFP.Records.AFP.CPD
import OpenAFP.Records.AFP.CPI
import OpenAFP.Records.AFP.CTC
import OpenAFP.Records.AFP.DXD
import OpenAFP.Records.AFP.EAG
import OpenAFP.Records.AFP.EBC
import OpenAFP.Records.AFP.ECA
import OpenAFP.Records.AFP.ECF
import OpenAFP.Records.AFP.ECP
import OpenAFP.Records.AFP.EDG
import OpenAFP.Records.AFP.EDI
import OpenAFP.Records.AFP.EDM
import OpenAFP.Records.AFP.EDT
import OpenAFP.Records.AFP.EDX
import OpenAFP.Records.AFP.EFG
import OpenAFP.Records.AFP.EFM
import OpenAFP.Records.AFP.EFN
import OpenAFP.Records.AFP.EGR
import OpenAFP.Records.AFP.EII
import OpenAFP.Records.AFP.EIM
import OpenAFP.Records.AFP.EMM
import OpenAFP.Records.AFP.EMO
import OpenAFP.Records.AFP.ENG
import OpenAFP.Records.AFP.EOC
import OpenAFP.Records.AFP.EOG
import OpenAFP.Records.AFP.EPG
import OpenAFP.Records.AFP.EPM
import OpenAFP.Records.AFP.EPS
import OpenAFP.Records.AFP.EPT
import OpenAFP.Records.AFP.ER
import OpenAFP.Records.AFP.ERG
import OpenAFP.Records.AFP.ESG
import OpenAFP.Records.AFP.FGD
import OpenAFP.Records.AFP.FNC
import OpenAFP.Records.AFP.FND
import OpenAFP.Records.AFP.FNG
import OpenAFP.Records.AFP.FNI
import OpenAFP.Records.AFP.FNM
import OpenAFP.Records.AFP.FNN
import OpenAFP.Records.AFP.FNO
import OpenAFP.Records.AFP.FNP
import OpenAFP.Records.AFP.GAD
import OpenAFP.Records.AFP.GDD
import OpenAFP.Records.AFP.ICP
import OpenAFP.Records.AFP.IDD
import OpenAFP.Records.AFP.IEL
import OpenAFP.Records.AFP.IID
import OpenAFP.Records.AFP.IMM
import OpenAFP.Records.AFP.IOB
import OpenAFP.Records.AFP.IOC
import OpenAFP.Records.AFP.IPD
import OpenAFP.Records.AFP.IPG
import OpenAFP.Records.AFP.IPO
import OpenAFP.Records.AFP.IPS
import OpenAFP.Records.AFP.IRD
import OpenAFP.Records.AFP.LLE
import OpenAFP.Records.AFP.LNC
import OpenAFP.Records.AFP.LND
import OpenAFP.Records.AFP.MBC
import OpenAFP.Records.AFP.MCA
import OpenAFP.Records.AFP.MCC
import OpenAFP.Records.AFP.MCD
import OpenAFP.Records.AFP.MCF
import OpenAFP.Records.AFP.MCF1
import OpenAFP.Records.AFP.MDD
import OpenAFP.Records.AFP.MDR
import OpenAFP.Records.AFP.MFC
import OpenAFP.Records.AFP.MGO
import OpenAFP.Records.AFP.MIO
import OpenAFP.Records.AFP.MMC
import OpenAFP.Records.AFP.MMO
import OpenAFP.Records.AFP.MMT
import OpenAFP.Records.AFP.MPG
import OpenAFP.Records.AFP.MPO
import OpenAFP.Records.AFP.MPS
import OpenAFP.Records.AFP.MSU
import OpenAFP.Records.AFP.NOP
import OpenAFP.Records.AFP.OBD
import OpenAFP.Records.AFP.OBP
import OpenAFP.Records.AFP.OCD
import OpenAFP.Records.AFP.PFC
import OpenAFP.Records.AFP.PGD
import OpenAFP.Records.AFP.PGP
import OpenAFP.Records.AFP.PGP1
import OpenAFP.Records.AFP.PMC
import OpenAFP.Records.AFP.PTD
import OpenAFP.Records.AFP.PTD1
import OpenAFP.Records.AFP.PTX
import OpenAFP.Records.AFP.TLE

_BAG  :: BAG
_BAG  = BAG  0xD3A8C9 0 _NStr
_BBC  :: BBC
_BBC  = BBC  0xD3A8EB 0 _NStr
_BCA  :: BCA
_BCA  = BCA  0xD3A877 0 _NStr
_BCF  :: BCF
_BCF  = BCF  0xD3A88A 0 _NStr
_BCP  :: BCP
_BCP  = BCP  0xD3A887 0 _NStr
_BDA  :: BDA
_BDA  = BDA  0xD3EEEB 0 _NStr
_BDD  :: BDD
_BDD  = BDD  0xD3A6EB 0 _NStr
_BDG  :: BDG
_BDG  = BDG  0xD3A8C4 0 _NStr
_BDI  :: BDI
_BDI  = BDI  0xD3A8A7 0 _NStr
_BDM  :: BDM
_BDM  = BDM  0xD3A8CA 0 _NStr
_BDT  :: BDT
_BDT  = BDT  0xD3A8A8 0 _NStr
_BDX  :: BDX
_BDX  = BDX  0xD3A8E3 0 _NStr
_BFG  :: BFG
_BFG  = BFG  0xD3A8C5 0 _NStr
_BFM  :: BFM
_BFM  = BFM  0xD3A8CD 0 _NStr
_BFN  :: BFN
_BFN  = BFN  0xD3A889 0 _NStr
_BGR  :: BGR
_BGR  = BGR  0xD3A8BB 0 _NStr
_BII  :: BII
_BII  = BII  0xD3A87B 0 0
_BIM  :: BIM
_BIM  = BIM  0xD3A8FB 0 _NStr
_BMM  :: BMM
_BMM  = BMM  0xD3A8CC 0 _NStr
_BMO  :: BMO
_BMO  = BMO  0xD3A8DF 0 _NStr
_BNG  :: BNG
_BNG  = BNG  0xD3A8AD 0 _NStr
_BOC  :: BOC
_BOC  = BOC  0xD3A892 0 _NStr
_BOG  :: BOG
_BOG  = BOG  0xD3A8C7 0 _NStr
_BPG  :: BPG
_BPG  = BPG  0xD3A8AF 0 _NStr
_BPM  :: BPM
_BPM  = BPM  0xD3A8CB 0 _NStr
_BPS  :: BPS
_BPS  = BPS  0xD3A85F 0 _NStr
_BPT  :: BPT
_BPT  = BPT  0xD3A89B 0 _NStr
_BR   :: BR
_BR   = BR   0xD3A8CE 0 _NStr
_BRG  :: BRG
_BRG  = BRG  0xD3A8C6 0 _NStr
_BSG  :: BSG
_BSG  = BSG  0xD3A8D9 0 _NStr
_CAT  :: CAT
_CAT  = CAT  0xD3B077 0 _NStr
_CDD  :: CDD
_CDD  = CDD  0xD3A692 0 _NStr
_CFC  :: CFC
_CFC  = CFC  0xD3A78A 0 0 _NStr
_CFI  :: CFI
_CFI  = CFI  0xD38C8A 0 []
_CFI_Data :: CFI_Data
_CFI_Data = CFI_Data 0 0 0 0
_CPC  :: CPC
_CPC  = CPC  0xD3A787 0 0 0 0 0 0 _NStr
_CPD  :: CPD
_CPD  = CPD  0xD3A687 0 _NStr
_CPI  :: CPI
_CPI  = CPI  0xD38C87 0 []
_CPI_Data :: CPI_Data
_CPI_Data = CPI_Data 0 0 0
_CTC  :: CTC
_CTC  = CTC  0xD3A79B 0 _NStr
_DXD  :: DXD
_DXD  = DXD  0xD3A6E3 0 _NStr
_EAG  :: EAG
_EAG  = EAG  0xD3A9C9 0 _NStr
_EBC  :: EBC
_EBC  = EBC  0xD3A9EB 0 _NStr
_ECA  :: ECA
_ECA  = ECA  0xD3A977 0 _NStr
_ECF  :: ECF
_ECF  = ECF  0xD3A98A 0 _NStr
_ECP  :: ECP
_ECP  = ECP  0xD3A987 0 _NStr
_EDG  :: EDG
_EDG  = EDG  0xD3A9C4 0 _NStr
_EDI  :: EDI
_EDI  = EDI  0xD3A9A7 0 _NStr
_EDM  :: EDM
_EDM  = EDM  0xD3A9CA 0 _NStr
_EDT  :: EDT
_EDT  = EDT  0xD3A9A8 0 _NStr
_EDX  :: EDX
_EDX  = EDX  0xD3A9E3 0 _NStr
_EFG  :: EFG
_EFG  = EFG  0xD3A9C5 0 _NStr
_EFM  :: EFM
_EFM  = EFM  0xD3A9CD 0 _NStr
_EFN  :: EFN
_EFN  = EFN  0xD3A989 0 _NStr
_EGR  :: EGR
_EGR  = EGR  0xD3A9BB 0 _NStr
_EII  :: EII
_EII  = EII  0xD3A97B 0 0
_EIM  :: EIM
_EIM  = EIM  0xD3A9FB 0 _NStr
_EMM  :: EMM
_EMM  = EMM  0xD3A9CC 0 _NStr
_EMO  :: EMO
_EMO  = EMO  0xD3A9DF 0 _NStr
_ENG  :: ENG
_ENG  = ENG  0xD3A9AD 0 _NStr
_EOC  :: EOC
_EOC  = EOC  0xD3A992 0 _NStr
_EOG  :: EOG
_EOG  = EOG  0xD3A9C7 0 _NStr
_EPG  :: EPG
_EPG  = EPG  0xD3A9AF 0 _NStr
_EPM  :: EPM
_EPM  = EPM  0xD3A9CB 0 _NStr
_EPS  :: EPS
_EPS  = EPS  0xD3A95F 0 _NStr
_EPT  :: EPT
_EPT  = EPT  0xD3A99B 0 _NStr
_ER   :: ER
_ER   = ER   0xD3A9CE 0 _NStr
_ERG  :: ERG
_ERG  = ERG  0xD3A9C6 0 _NStr
_ESG  :: ESG
_ESG  = ESG  0xD3A9D9 0 _NStr
_FGD  :: FGD
_FGD  = FGD  0xD3A6C5 0 _NStr
_FNC  :: FNC
_FNC  = FNC  0xD3A789 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _NStr
_FND  :: FND
_FND  = FND  0xD3A689 0 _NStr
_FNG  :: FNG
_FNG  = FNG  0xD3EE89 0 _NStr
_FNI  :: FNI
_FNI  = FNI  0xD38C89 0 []
_FNI_Data :: FNI_Data
_FNI_Data = FNI_Data 0 0 0 0 0 0 0 0 0 0 0
_FNM  :: FNM
_FNM  = FNM  0xD3A289 0 []
_FNM_Data :: FNM_Data
_FNM_Data = FNM_Data 0 0 0
_FNN  :: FNN
_FNN  = FNN  0xD3AB89 0 _NStr
_FNO  :: FNO
_FNO  = FNO  0xD3AE89 0 0 0 0 0 0 _NStr
_FNP  :: FNP
_FNP  = FNP  0xD3AC89 0 _NStr
_GAD  :: GAD
_GAD  = GAD  0xD3EEBB 0 _NStr
_GDD  :: GDD
_GDD  = GDD  0xD3A6BB 0 _NStr
_ICP  :: ICP
_ICP  = ICP  0xD3AC7B 0 0 0 0 0 0 0
_IDD  :: IDD
_IDD  = IDD  0xD3A6FB 0 _NStr
_IEL  :: IEL
_IEL  = IEL  0xD3B2A7 0 _NStr
_IID  :: IID
_IID  = IID  0xD3A67B 0 0 0 0 0 0 0 0 0 0 0 0 0
_IMM  :: IMM
_IMM  = IMM  0xD3ABCC 0 _NStr
_IOB  :: IOB
_IOB  = IOB  0xD3AFC3 0 _NStr
_IOC  :: IOC
_IOC  = IOC  0xD3A77B 0 0 0 0 0 0 0 0 0 0 0
_IPD  :: IPD
_IPD  = IPD  0xD3EEFB 0 _NStr
_IPG  :: IPG
_IPG  = IPG  0xD3AFAF 0 _NStr
_IPO  :: IPO
_IPO  = IPO  0xD3AFD8 0 _NStr
_IPS  :: IPS
_IPS  = IPS  0xD3AF5F 0 _NStr
_IRD  :: IRD
_IRD  = IRD  0xD3EE7B 0 _NStr
_LLE  :: LLE
_LLE  = LLE  0xD3B490 0 _NStr
_LNC  :: LNC
_LNC  = LNC  0xD3AAE7 0 _NStr
_LND  :: LND
_LND  = LND  0xD3A6E7 0 _NStr
_MBC  :: MBC
_MBC  = MBC  0xD3ABEB 0 _NStr
_MCA  :: MCA
_MCA  = MCA  0xD3AB77 0 _NStr
_MCC  :: MCC
_MCC  = MCC  0xD3A288 0 _NStr
_MCD  :: MCD
_MCD  = MCD  0xD3AB92 0 _NStr
_MCF  :: MCF
_MCF  = MCF  0xD3AB8A 0 []
_MCF1 :: MCF1
_MCF1 = MCF1 0xD3B18A 0 0 0 []
_MCF1_Data :: MCF1_Data
_MCF1_Data = MCF1_Data 0 0 0 0 0 0 0 0
_MDD  :: MDD
_MDD  = MDD  0xD3A688 0 _NStr
_MDR  :: MDR
_MDR  = MDR  0xD3ABC3 0 _NStr
_MFC  :: MFC
_MFC  = MFC  0xD3A088 0 _NStr
_MGO  :: MGO
_MGO  = MGO  0xD3ABBB 0 _NStr
_MIO  :: MIO
_MIO  = MIO  0xD3ABFB 0 _NStr
_MMC  :: MMC
_MMC  = MMC  0xD3A788 0 _NStr
_MMO  :: MMO
_MMO  = MMO  0xD3B1DF 0 _NStr
_MMT  :: MMT
_MMT  = MMT  0xD3AB88 0 _NStr
_MPG  :: MPG
_MPG  = MPG  0xD3ABAF 0 _NStr
_MPO  :: MPO
_MPO  = MPO  0xD3ABD8 0 _NStr
_MPS  :: MPS
_MPS  = MPS  0xD3B15F 0 _NStr
_MSU  :: MSU
_MSU  = MSU  0xD3ABEA 0 _NStr
_NOP  :: NOP
_NOP  = NOP  0xD3EEEE 0 _NStr
_OBD  :: OBD
_OBD  = OBD  0xD3A66B 0 _NStr
_OBP  :: OBP
_OBP  = OBP  0xD3AC6B 0 _NStr
_OCD  :: OCD
_OCD  = OCD  0xD3EE92 0 _NStr
_PFC  :: PFC
_PFC  = PFC  0xD3B288 0 _NStr
_PGD  :: PGD
_PGD  = PGD  0xD3A6AF 0 0 0 0 0 0 0 0 0 _NStr
_PGP  :: PGP
_PGP  = PGP  0xD3B1AF 0 _NStr
_PGP1 :: PGP1
_PGP1 = PGP1 0xD3ACAF 0 _NStr
_PMC  :: PMC
_PMC  = PMC  0xD3A7AF 0 _NStr
_PTD  :: PTD
_PTD  = PTD  0xD3B19B 0 _NStr
_PTD1 :: PTD1
_PTD1 = PTD1 0xD3A69B 0 _NStr
_PTX  :: PTX
_PTX  = PTX  0xD3EE9B 0 0 []
_TLE  :: TLE
_TLE  = TLE  0xD3A090 0 []

