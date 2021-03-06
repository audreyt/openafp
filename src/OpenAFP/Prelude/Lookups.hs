
module OpenAFP.Prelude.Lookups where
import OpenAFP.Types
import OpenAFP.Records
import OpenAFP.Internals
import OpenAFP.Prelude.Instances
import qualified Data.Map as Map

descLookup :: ChunkType -> String
descLookup typ = Map.findWithDefault "(Unknown)" typ descMap

descMap :: Map.Map ChunkType String
descMap = Map.fromList
    [ (chunkTypeOf _MCF_T, "MCF Triplets")
    , (chunkTypeOf _CPI, "Code Page Index")
    , (chunkTypeOf _FNI, "Font Index")
    , (chunkTypeOf _CFI, "Coded Font Index")
    , (chunkTypeOf _MFC, "Medium Finishing Control")
    , (chunkTypeOf _TLE, "Tag Logical Element")
    , (chunkTypeOf _MCC, "Medium Copy Count")
    , (chunkTypeOf _FNM, "Font Patterns Map")
    , (chunkTypeOf _OBD, "Object Area Descriptor")
    , (chunkTypeOf _IID, "IM Image Input Descriptor (C)")
    , (chunkTypeOf _CPD, "Code Page Descriptor")
    , (chunkTypeOf _MDD, "Medium Descriptor")
    , (chunkTypeOf _FND, "Font Descriptor")
    , (chunkTypeOf _CDD, "Container Data Descriptor")
    , (chunkTypeOf _PTD1, "Presentation Text Descriptor Format-1 (C)")
    , (chunkTypeOf _PGD, "Page Descriptor")
    , (chunkTypeOf _GDD, "Graphics Data Descriptor")
    , (chunkTypeOf _FGD, "Form Environment Group Descriptor (O)")
    , (chunkTypeOf _DXD, "Data Map Transmission Subcase Descriptor")
    , (chunkTypeOf _LND, "Line Descriptor")
    , (chunkTypeOf _BDD, "Bar Code Data Descriptor")
    , (chunkTypeOf _IDD, "Image Data Descriptor")
    , (chunkTypeOf _IOC, "IM Image Output Control (C)")
    , (chunkTypeOf _CPC, "Code Page Control")
    , (chunkTypeOf _MMC, "Medium Modification Control")
    , (chunkTypeOf _FNC, "Font Control")
    , (chunkTypeOf _CFC, "Coded Font Control")
    , (chunkTypeOf _CTC, "Composed Text Control (O)")
    , (chunkTypeOf _PMC, "Page Modification Control")
    , (chunkTypeOf _BPS, "Begin Page Segment")
    , (chunkTypeOf _BCA, "Begin Color Attribute Table")
    , (chunkTypeOf _BII, "Begin IM Image (C)")
    , (chunkTypeOf _BCP, "Begin Code Page")
    , (chunkTypeOf _BFN, "Begin Font")
    , (chunkTypeOf _BCF, "Begin Coded Font")
    , (chunkTypeOf _BOC, "Begin Object Container")
    , (chunkTypeOf _BPT, "Begin Presentation Text Object")
    , (chunkTypeOf _BDI, "Begin Document Index")
    , (chunkTypeOf _BDT, "Begin Document")
    , (chunkTypeOf _BNG, "Begin Named Page Group")
    , (chunkTypeOf _BPG, "Begin Page")
    , (chunkTypeOf _BGR, "Begin Graphics Object")
    , (chunkTypeOf _BDG, "Begin Document Environment Group")
    , (chunkTypeOf _BFG, "Begin Form Environment Group (O)")
    , (chunkTypeOf _BRG, "Begin Resource Group")
    , (chunkTypeOf _BOG, "Begin Object Environment Group")
    , (chunkTypeOf _BAG, "Begin Active Environment Group")
    , (chunkTypeOf _BDM, "Begin Data Map")
    , (chunkTypeOf _BPM, "Begin Page Map")
    , (chunkTypeOf _BMM, "Begin Medium Map")
    , (chunkTypeOf _BFM, "Begin Form Map")
    , (chunkTypeOf _BR, "Begin Resource (R)")
    , (chunkTypeOf _BSG, "Begin Resource Environment Group")
    , (chunkTypeOf _BMO, "Begin Overlay")
    , (chunkTypeOf _BDX, "Begin Data Map Transmission Subcase")
    , (chunkTypeOf _BBC, "Begin Bar Code Object")
    , (chunkTypeOf _BIM, "Begin Image Object")
    , (chunkTypeOf _EPS, "End Page Segment")
    , (chunkTypeOf _ECA, "End Color Attribute Table")
    , (chunkTypeOf _EII, "End IM Image (C)")
    , (chunkTypeOf _ECP, "End Code Page")
    , (chunkTypeOf _EFN, "End Font")
    , (chunkTypeOf _ECF, "End Coded Font")
    , (chunkTypeOf _EOC, "End Object Container")
    , (chunkTypeOf _EPT, "End Presentation Text Object")
    , (chunkTypeOf _EDI, "End Document Index")
    , (chunkTypeOf _EDT, "End Document")
    , (chunkTypeOf _ENG, "End Named Page Group")
    , (chunkTypeOf _EPG, "End Page")
    , (chunkTypeOf _EGR, "End Graphics Object")
    , (chunkTypeOf _EDG, "End Document Environment Group")
    , (chunkTypeOf _EFG, "End Form Environment Group (O)")
    , (chunkTypeOf _ERG, "End Resource Group")
    , (chunkTypeOf _EOG, "End Object Environment Group")
    , (chunkTypeOf _EAG, "End Active Environment Group")
    , (chunkTypeOf _EDM, "End Data Map")
    , (chunkTypeOf _EPM, "End Page Map")
    , (chunkTypeOf _EMM, "End Medium Map")
    , (chunkTypeOf _EFM, "End Form Map")
    , (chunkTypeOf _ER, "End Resource (R)")
    , (chunkTypeOf _ESG, "End Resource Environment Group")
    , (chunkTypeOf _EMO, "End Overlay")
    , (chunkTypeOf _EDX, "End Data Map Transmission Subcase")
    , (chunkTypeOf _EBC, "End Bar Code Object")
    , (chunkTypeOf _EIM, "End Image Object")
    , (chunkTypeOf _LNC, "Line Descriptor Count")
    , (chunkTypeOf _MCA, "Map Color Attribute Table")
    , (chunkTypeOf _MMT, "Map Media Type")
    , (chunkTypeOf _FNN, "Font Names (Outline Fonts Only)")
    , (chunkTypeOf _MCF, "Map Coded Font")
    , (chunkTypeOf _MCD, "Map Container Data")
    , (chunkTypeOf _MPG, "Map Page")
    , (chunkTypeOf _MGO, "Map Graphics Object")
    , (chunkTypeOf _MDR, "Map Data Resource")
    , (chunkTypeOf _IMM, "Invoke Medium Map")
    , (chunkTypeOf _MPO, "Map Page Overlay")
    , (chunkTypeOf _MSU, "Map Suppression")
    , (chunkTypeOf _MBC, "Map Bar Code Object")
    , (chunkTypeOf _MIO, "Map Image Object")
    , (chunkTypeOf _OBP, "Object Area Position")
    , (chunkTypeOf _ICP, "IM Image Cell Position (C)")
    , (chunkTypeOf _FNP, "Font Position")
    , (chunkTypeOf _PGP1, "Page Position Format-1 (C)")
    , (chunkTypeOf _FNO, "Font Orientation")
    , (chunkTypeOf _IPS, "Include Page Segment")
    , (chunkTypeOf _IPG, "Include Page")
    , (chunkTypeOf _IOB, "Include Object")
    , (chunkTypeOf _IPO, "Include Page Overlay")
    , (chunkTypeOf _CAT, "Color Attribute Table")
    , (chunkTypeOf _MPS, "Map Page Segment")
    , (chunkTypeOf _MCF1, "Map Coded Font Format-1 (C)")
    , (chunkTypeOf _PTD, "Presentation Text Data Descriptor")
    , (chunkTypeOf _PGP, "Page Position")
    , (chunkTypeOf _MMO, "Map Medium Overlay")
    , (chunkTypeOf _PFC, "Presentation Fidelity Control")
    , (chunkTypeOf _IEL, "Index Element")
    , (chunkTypeOf _LLE, "Link Logical Element")
    , (chunkTypeOf _IRD, "IM Image Raster Data (C)")
    , (chunkTypeOf _FNG, "Font Patterns")
    , (chunkTypeOf _OCD, "Object Container Data")
    , (chunkTypeOf _PTX, "Presentation Text Data")
    , (chunkTypeOf _GAD, "Graphics Data")
    , (chunkTypeOf _BDA, "Bar Code Data")
    , (chunkTypeOf _NOP, "No Operation")
    , (chunkTypeOf _IPD, "Image Picture Data")
    , (chunkTypeOf _T_CGCSGI, "Coded Graphic Character Set Global ID")
    , (chunkTypeOf _T_FQN, "Fully Qualified Name")
    , (chunkTypeOf _T_MO, "Mapping Option")
    , (chunkTypeOf _T_OCL, "Object Classification")
    , (chunkTypeOf _T_MIS, "MO:DCA Interchange Set")
    , (chunkTypeOf _T_TO, "Text Orientation (R)")
    , (chunkTypeOf _T_FDS, "Font Descriptor Specification")
    , (chunkTypeOf _T_FCGCSGI, "Font Coded Graphic Character Set Global Identifier")
    , (chunkTypeOf _T_OFSS, "Object Function Set Specification")
    , (chunkTypeOf _T_ROT, "Resource Object Type (R)")
    , (chunkTypeOf _T_ERLI, "Extended Resource Local ID")
    , (chunkTypeOf _T_RLI, "Resource Local ID")
    , (chunkTypeOf _T_RSN, "Resource Section Number")
    , (chunkTypeOf _T_CR, "Character Rotation")
    , (chunkTypeOf _T_LDOPM, "Line Data Object Position Migration (R)")
    , (chunkTypeOf _T_OBO, "Object Byte Offset")
    , (chunkTypeOf _T_AV, "Attribute Value")
    , (chunkTypeOf _T_DP, "Descriptor Position")
    , (chunkTypeOf _T_MEC, "Media Eject Control")
    , (chunkTypeOf _T_POCP, "Page Overlay Conditional Processing")
    , (chunkTypeOf _T_RUA, "Resource Usage Attribute")
    , (chunkTypeOf _T_OAMU, "Object Area Measurement Units")
    , (chunkTypeOf _T_OAS, "Object Area Size")
    , (chunkTypeOf _T_AD, "Area Definition")
    , (chunkTypeOf _T_CS, "Color Specification")
    , (chunkTypeOf _T_ESI, "Encoding Scheme ID")
    , (chunkTypeOf _T_MMPN, "Medium Map Page Number")
    , (chunkTypeOf _T_OBE, "Object Byte Extent")
    , (chunkTypeOf _T_OSFO, "Object Structured Field Offset")
    , (chunkTypeOf _T_OSFE, "Object Structured Field Extent")
    , (chunkTypeOf _T_OO, "Object Offset")
    , (chunkTypeOf _T_FHSF, "Font Horizontal Scale Factor")
    , (chunkTypeOf _T_OCO, "Object Count")
    , (chunkTypeOf _T_LDTS, "Local Date and Time Stamp")
    , (chunkTypeOf _T_OCH, "Object Checksum (R)")
    , (chunkTypeOf _T_T1CRMT, "Type 1 - CRC Resource Management Triplet")
    , (chunkTypeOf _T_T2FRMT, "Type 2 - Font Resource Management Triplet")
    , (chunkTypeOf _T_OOI, "Object Origin Identifier (R)")
    , (chunkTypeOf _T_C, "Comment")
    , (chunkTypeOf _T_MOR, "Medium Orientation")
    , (chunkTypeOf _T_ROI, "Resource Object Include")
    , (chunkTypeOf _T_EF, "Extension Font")
    , (chunkTypeOf _T_PSRM, "Presentation Space Reset Mixing")
    , (chunkTypeOf _T_PSMR, "Presentation Space Mixing Rules")
    , (chunkTypeOf _T_UDTS, "Universal Date and Time Stamp")
    , (chunkTypeOf _T_II, "IMM Insertion (R)")
    , (chunkTypeOf _T_TS, "Toner Saver")
    , (chunkTypeOf _T_CF, "Color Fidelity")
    , (chunkTypeOf _T_FF, "Font Fidelity")
    , (chunkTypeOf _T_MA, "Metric Adjustment")
    , (chunkTypeOf _T_AQ, "Attribute Qualifier")
    , (chunkTypeOf _T_PPI, "Page Position Information")
    , (chunkTypeOf _T_PV, "Parameter Value")
    , (chunkTypeOf _T_PC, "Presentation Control")
    , (chunkTypeOf _T_FRMT, "Font Resolution and Metric Technology")
    , (chunkTypeOf _T_FO, "Finishing Operation")
    , (chunkTypeOf _T_MF, "Media Fidelity")
    , (chunkTypeOf _PTX_STC, "Set Text Color")
    , (chunkTypeOf _PTX_SIM, "Set Inline Margin")
    , (chunkTypeOf _PTX_SIA, "Set Intercharacter Adjustment")
    , (chunkTypeOf _PTX_SVI, "Set Variable-Space Character Increment")
    , (chunkTypeOf _PTX_AMI, "Absolute Move Inline")
    , (chunkTypeOf _PTX_RMI, "Relative Move Inline")
    , (chunkTypeOf _PTX_SBI, "Set Baseline Increment")
    , (chunkTypeOf _PTX_AMB, "Absolute Move Baseline")
    , (chunkTypeOf _PTX_RMB, "Relative Move Baseline")
    , (chunkTypeOf _PTX_BLN, "Begin Line Next")
    , (chunkTypeOf _PTX_DIR, "Draw I-Axis Rule")
    , (chunkTypeOf _PTX_DBR, "Draw B-Axis Rule")
    , (chunkTypeOf _PTX_RPS, "Repeat String")
    , (chunkTypeOf _PTX_SCFL, "Set Coded Font Local")
    , (chunkTypeOf _PTX_BSU, "Begin Suppression")
    , (chunkTypeOf _PTX_ESU, "Begin Suppression")
    , (chunkTypeOf _PTX_STO, "Set Text Orientation")
    , (chunkTypeOf _PTX_NOP, "No Operation")
    , (chunkTypeOf _PTX_TRN, "Transparent Data")
    ]
