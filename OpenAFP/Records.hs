{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Records
-- Copyright   :  (c) Autrijus Tang 2004
-- License     :  BSD-style
-- 
-- Maintainer  :  autrijus@autrijus.org
-- Stability   :  experimental
-- Portability :  non-portable (GHC-only)
--
-- This module imports and re-exports various AFP data structures, and
-- implements an additional "Unknown" type for data without corresponding
-- structures.
--
-----------------------------------------------------------------------------

module OpenAFP.Records (
    module OpenAFP.Records.AFP,
    module OpenAFP.Records.PTX,
    module OpenAFP.Records.MCF,
    module OpenAFP.Records.T,
    AFP_(..),
    Unknown(..), _Unknown,

    lookupAFP, lookupMCF, lookupPTX, lookupT,
) where 
import OpenAFP.Types
import OpenAFP.Internals
import OpenAFP.Records.AFP
import OpenAFP.Records.PTX
import OpenAFP.Records.MCF
import OpenAFP.Records.T

---[ Unknown ]--------------------------------------------------

newtype Unknown = Unknown NStr
    deriving (Show, Typeable)

instance Rec Unknown where
    recGet bh = get bh >>= return . Unknown
    recPut bh (Unknown r) = put bh r
    recSizeOf (Unknown r) = sizeOf r
    recView (Unknown r) = viewRecord (typeOf _Unknown) [ viewField "" (viewNStr r) ]

instance Binary Unknown where
    get bh = return $ Unknown $ bufFromPStrLen (bufOf bh)
    put bh (Unknown buf) = putBuf bh $ bufToPStrLen buf

_Unknown = Unknown _NStr

---[ AFP ]-----------------------------------------------------

newtype AFP_ = AFP_ (N3, Buffer2) deriving (Show, Typeable)

instance Binary AFP_ where
    put bh (AFP_ (_, buf)) = do
        put bh ('\x5A', buf)
    get bh = do
        getByte bh
	get' AFP_ bh

lookupMCF :: MCF_ -> N0 -> ChunkType
lookupMCF _ _ = typeOf _MCF_T    -- MCF Triplets

lookupAFP :: AFP_ -> N3 -> ChunkType
lookupAFP _ x = case x of
    0xD38C87 -> typeOf _CPI      -- Code Page Index
    0xD38C89 -> typeOf _FNI      -- Font Index
    0xD38C8A -> typeOf _CFI      -- Coded Font Index
    0xD3A088 -> typeOf _MFC      -- Medium Finishing Control
    0xD3A090 -> typeOf _TLE      -- Tag Logical Element
    0xD3A288 -> typeOf _MCC      -- Medium Copy Count
    0xD3A289 -> typeOf _FNM      -- Font Patterns Map
    0xD3A66B -> typeOf _OBD      -- Object Area Descriptor
    0xD3A67B -> typeOf _IID      -- IM Image Input Descriptor (C)
    0xD3A687 -> typeOf _CPD      -- Code Page Descriptor
    0xD3A688 -> typeOf _MDD      -- Medium Descriptor
    0xD3A689 -> typeOf _FND      -- Font Descriptor
    0xD3A692 -> typeOf _CDD      -- Container Data Descriptor
    0xD3A69B -> typeOf _PTD1     -- Presentation Text Descriptor Format-1 (C)
    0xD3A6AF -> typeOf _PGD      -- Page Descriptor
    0xD3A6BB -> typeOf _GDD      -- Graphics Data Descriptor
    0xD3A6C5 -> typeOf _FGD      -- Form Environment Group Descriptor (O)
    0xD3A6E3 -> typeOf _DXD      -- Data Map Transmission Subcase Descriptor
    0xD3A6E7 -> typeOf _LND      -- Line Descriptor
    0xD3A6EB -> typeOf _BDD      -- Bar Code Data Descriptor
    0xD3A6FB -> typeOf _IDD      -- Image Data Descriptor
    0xD3A77B -> typeOf _IOC      -- IM Image Output Control (C)
    0xD3A787 -> typeOf _CPC      -- Code Page Control
    0xD3A788 -> typeOf _MMC      -- Medium Modification Control
    0xD3A789 -> typeOf _FNC      -- Font Control
    0xD3A78A -> typeOf _CFC      -- Coded Font Control
    0xD3A79B -> typeOf _CTC      -- Composed Text Control (O)
    0xD3A7AF -> typeOf _PMC      -- Page Modification Control
    0xD3A85F -> typeOf _BPS      -- Begin Page Segment
    0xD3A877 -> typeOf _BCA      -- Begin Color Attribute Table
    0xD3A87B -> typeOf _BII      -- Begin IM Image (C)
    0xD3A887 -> typeOf _BCP      -- Begin Code Page
    0xD3A889 -> typeOf _BFN      -- Begin Font
    0xD3A88A -> typeOf _BCF      -- Begin Coded Font
    0xD3A892 -> typeOf _BOC      -- Begin Object Container
    0xD3A89B -> typeOf _BPT      -- Begin Presentation Text Object
    0xD3A8A7 -> typeOf _BDI      -- Begin Document Index
    0xD3A8A8 -> typeOf _BDT      -- Begin Document
    0xD3A8AD -> typeOf _BNG      -- Begin Named Page Group
    0xD3A8AF -> typeOf _BPG      -- Begin Page
    0xD3A8BB -> typeOf _BGR      -- Begin Graphics Object
    0xD3A8C4 -> typeOf _BDG      -- Begin Document Environment Group
    0xD3A8C5 -> typeOf _BFG      -- Begin Form Environment Group (O)
    0xD3A8C6 -> typeOf _BRG      -- Begin Resource Group
    0xD3A8C7 -> typeOf _BOG      -- Begin Object Environment Group
    0xD3A8C9 -> typeOf _BAG      -- Begin Active Environment Group
    0xD3A8CA -> typeOf _BDM      -- Begin Data Map
    0xD3A8CB -> typeOf _BPM      -- Begin Page Map
    0xD3A8CC -> typeOf _BMM      -- Begin Medium Map
    0xD3A8CD -> typeOf _BFM      -- Begin Form Map
    0xD3A8CE -> typeOf _BR       -- Begin Resource (R)
    0xD3A8D9 -> typeOf _BSG      -- Begin Resource Environment Group
    0xD3A8DF -> typeOf _BMO      -- Begin Overlay
    0xD3A8E3 -> typeOf _BDX      -- Begin Data Map Transmission Subcase
    0xD3A8EB -> typeOf _BBC      -- Begin Bar Code Object
    0xD3A8FB -> typeOf _BIM      -- Begin Image Object
    0xD3A95F -> typeOf _EPS      -- End Page Segment
    0xD3A977 -> typeOf _ECA      -- End Color Attribute Table
    0xD3A97B -> typeOf _EII      -- End IM Image (C)
    0xD3A987 -> typeOf _ECP      -- End Code Page
    0xD3A989 -> typeOf _EFN      -- End Font
    0xD3A98A -> typeOf _ECF      -- End Coded Font
    0xD3A992 -> typeOf _EOC      -- End Object Container
    0xD3A99B -> typeOf _EPT      -- End Presentation Text Object
    0xD3A9A7 -> typeOf _EDI      -- End Document Index
    0xD3A9A8 -> typeOf _EDT      -- End Document
    0xD3A9AD -> typeOf _ENG      -- End Named Page Group
    0xD3A9AF -> typeOf _EPG      -- End Page
    0xD3A9BB -> typeOf _EGR      -- End Graphics Object
    0xD3A9C4 -> typeOf _EDG      -- End Document Environment Group
    0xD3A9C5 -> typeOf _EFG      -- End Form Environment Group (O)
    0xD3A9C6 -> typeOf _ERG      -- End Resource Group
    0xD3A9C7 -> typeOf _EOG      -- End Object Environment Group
    0xD3A9C9 -> typeOf _EAG      -- End Active Environment Group
    0xD3A9CA -> typeOf _EDM      -- End Data Map
    0xD3A9CB -> typeOf _EPM      -- End Page Map
    0xD3A9CC -> typeOf _EMM      -- End Medium Map
    0xD3A9CD -> typeOf _EFM      -- End Form Map
    0xD3A9CE -> typeOf _ER       -- End Resource (R)
    0xD3A9D9 -> typeOf _ESG      -- End Resource Environment Group
    0xD3A9DF -> typeOf _EMO      -- End Overlay
    0xD3A9E3 -> typeOf _EDX      -- End Data Map Transmission Subcase
    0xD3A9EB -> typeOf _EBC      -- End Bar Code Object
    0xD3A9FB -> typeOf _EIM      -- End Image Object
    0xD3AAE7 -> typeOf _LNC      -- Line Descriptor Count
    0xD3AB77 -> typeOf _MCA      -- Map Color Attribute Table
    0xD3AB88 -> typeOf _MMT      -- Map Media Type
    0xD3AB89 -> typeOf _FNN      -- Font Names (Outline Fonts Only)
    0xD3AB8A -> typeOf _MCF      -- Map Coded Font
    0xD3AB92 -> typeOf _MCD      -- Map Container Data
    0xD3ABAF -> typeOf _MPG      -- Map Page
    0xD3ABBB -> typeOf _MGO      -- Map Graphics Object
    0xD3ABC3 -> typeOf _MDR      -- Map Data Resource
    0xD3ABCC -> typeOf _IMM      -- Invoke Medium Map
    0xD3ABD8 -> typeOf _MPO      -- Map Page Overlay
    0xD3ABEA -> typeOf _MSU      -- Map Suppression
    0xD3ABEB -> typeOf _MBC      -- Map Bar Code Object
    0xD3ABFB -> typeOf _MIO      -- Map Image Object
    0xD3AC6B -> typeOf _OBP      -- Object Area Position
    0xD3AC7B -> typeOf _ICP      -- IM Image Cell Position (C)
    0xD3AC89 -> typeOf _FNP      -- Font Position
    0xD3ACAF -> typeOf _PGP1     -- Page Position Format-1 (C)
    0xD3AE89 -> typeOf _FNO      -- Font Orientation
    0xD3AF5F -> typeOf _IPS      -- Include Page Segment
    0xD3AFAF -> typeOf _IPG      -- Include Page
    0xD3AFC3 -> typeOf _IOB      -- Include Object
    0xD3AFD8 -> typeOf _IPO      -- Include Page Overlay
    0xD3B077 -> typeOf _CAT      -- Color Attribute Table
    0xD3B15F -> typeOf _MPS      -- Map Page Segment
    0xD3B18A -> typeOf _MCF1     -- Map Coded Font Format-1 (C)
    0xD3B19B -> typeOf _PTD      -- Presentation Text Data Descriptor
    0xD3B1AF -> typeOf _PGP      -- Page Position
    0xD3B1DF -> typeOf _MMO      -- Map Medium Overlay
    0xD3B288 -> typeOf _PFC      -- Presentation Fidelity Control
    0xD3B2A7 -> typeOf _IEL      -- Index Element
    0xD3B490 -> typeOf _LLE      -- Link Logical Element
    0xD3EE7B -> typeOf _IRD      -- IM Image Raster Data (C)
    0xD3EE89 -> typeOf _FNG      -- Font Patterns
    0xD3EE92 -> typeOf _OCD      -- Object Container Data
    0xD3EE9B -> typeOf _PTX      -- Presentation Text Data
    0xD3EEBB -> typeOf _GAD      -- Graphics Data
    0xD3EEEB -> typeOf _BDA      -- Bar Code Data
    0xD3EEEE -> typeOf _NOP      -- No Operation
    0xD3EEFB -> typeOf _IPD      -- Image Picture Data
    _        -> typeOf _Unknown


---[ Triplets ]-------------------------------------------------

lookupT :: T_ -> N1 -> ChunkType
lookupT _ x = case x of
    0x01 -> typeOf _T_CGCSGI    -- Coded Graphic Character Set Global ID
    0x02 -> typeOf _T_FQN       -- Fully Qualified Name
    0x04 -> typeOf _T_MO        -- Mapping Option
    0x10 -> typeOf _T_OCL       -- Object Classification
    0x18 -> typeOf _T_MIS       -- MO:DCA Interchange Set
    0x1D -> typeOf _T_TO        -- Text Orientation (R)
    0x1F -> typeOf _T_FDS       -- Font Descriptor Specification
    0x20 -> typeOf _T_FCGCSGI   -- Font Coded Graphic Character Set Global Identifier
    0x21 -> typeOf _T_OFSS      -- Object Function Set Specification
    0x22 -> typeOf _T_ROT       -- Resource Object Type (R)
    0x23 -> typeOf _T_ERLI      -- Extended Resource Local ID
    0x24 -> typeOf _T_RLI       -- Resource Local ID
    0x25 -> typeOf _T_RSN       -- Resource Section Number
    0x26 -> typeOf _T_CR        -- Character Rotation
    0x27 -> typeOf _T_LDOPM     -- Line Data Object Position Migration (R)
    0x2D -> typeOf _T_OBO       -- Object Byte Offset
    0x36 -> typeOf _T_AV        -- Attribute Value
    0x43 -> typeOf _T_DP        -- Descriptor Position
    0x45 -> typeOf _T_MEC       -- Media Eject Control
    0x46 -> typeOf _T_POCP      -- Page Overlay Conditional Processing
    0x47 -> typeOf _T_RUA       -- Resource Usage Attribute
    0x4B -> typeOf _T_OAMU      -- Object Area Measurement Units
    0x4C -> typeOf _T_OAS       -- Object Area Size
    0x4D -> typeOf _T_AD        -- Area Definition
    0x4E -> typeOf _T_CS        -- Color Specification
    0x50 -> typeOf _T_ESI       -- Encoding Scheme ID
    0x56 -> typeOf _T_MMPN      -- Medium Map Page Number
    0x57 -> typeOf _T_OBE       -- Object Byte Extent
    0x58 -> typeOf _T_OSFO      -- Object Structured Field Offset
    0x59 -> typeOf _T_OSFE      -- Object Structured Field Extent
    0x5A -> typeOf _T_OO        -- Object Offset
    0x5D -> typeOf _T_FHSF      -- Font Horizontal Scale Factor
    0x5E -> typeOf _T_OCO       -- Object Count
    0x62 -> typeOf _T_LDTS      -- Local Date and Time Stamp
    0x63 -> typeOf _T_OCH       -- Object Checksum (R)
--  0x63 -> typeOf _T_T1CRMT    -- Type 1 - CRC Resource Management Triplet
--  0x63 -> typeOf _T_T2FRMT    -- Type 2 - Font Resource Management Triplet
    0x64 -> typeOf _T_OOI       -- Object Origin Identifier (R)
    0x65 -> typeOf _T_C         -- Comment
    0x68 -> typeOf _T_MOR       -- Medium Orientation
    0x6C -> typeOf _T_ROI       -- Resource Object Include
    0x6D -> typeOf _T_EF        -- Extension Font
    0x70 -> typeOf _T_PSRM      -- Presentation Space Reset Mixing
    0x71 -> typeOf _T_PSMR      -- Presentation Space Mixing Rules
    0x72 -> typeOf _T_UDTS      -- Universal Date and Time Stamp
    0x73 -> typeOf _T_II        -- IMM Insertion (R)
    0x74 -> typeOf _T_TS        -- Toner Saver
    0x75 -> typeOf _T_CF        -- Color Fidelity
    0x78 -> typeOf _T_FF        -- Font Fidelity
    0x79 -> typeOf _T_MA        -- Metric Adjustment
    0x80 -> typeOf _T_AQ        -- Attribute Qualifier
    0x81 -> typeOf _T_PPI       -- Page Position Information
    0x82 -> typeOf _T_PV        -- Parameter Value
    0x83 -> typeOf _T_PC        -- Presentation Control
    0x84 -> typeOf _T_FRMT      -- Font Resolution and Metric Technology
    0x85 -> typeOf _T_FO        -- Finishing Operation
    0x87 -> typeOf _T_MF        -- Media Fidelity
    _    -> typeOf _Unknown

---[ PTX ]------------------------------------------------------

lookupPTX :: PTX_ -> N1 -> ChunkType
lookupPTX _ x = case x of
    0x74 -> typeOf _PTX_STC      -- Set Text Color
    0x75 -> typeOf _PTX_STC
    0xC0 -> typeOf _PTX_SIM      -- Set Inline Margin
    0xC1 -> typeOf _PTX_SIM
    0xC2 -> typeOf _PTX_SIA      -- Set Intercharacter Adjustment
    0xC3 -> typeOf _PTX_SIA
    0xC4 -> typeOf _PTX_SVI      -- Set Variable-Space Character Increment
    0xC5 -> typeOf _PTX_SVI
    0xC6 -> typeOf _PTX_AMI      -- Absolute Move Inline
    0xC7 -> typeOf _PTX_AMI
    0xC8 -> typeOf _PTX_RMI      -- Relative Move Inline
    0xC9 -> typeOf _PTX_RMI
    0xD0 -> typeOf _PTX_SBI      -- Set Baseline Increment
    0xD1 -> typeOf _PTX_SBI
    0xD2 -> typeOf _PTX_AMB      -- Absolute Move Baseline
    0xD3 -> typeOf _PTX_AMB
    0xD4 -> typeOf _PTX_RMB      -- Relative Move Baseline
    0xD5 -> typeOf _PTX_RMB
    0xD8 -> typeOf _PTX_BLN      -- Begin Line Next
    0xD9 -> typeOf _PTX_BLN
    0xE4 -> typeOf _PTX_DIR      -- Draw I-Axis Rule
    0xE5 -> typeOf _PTX_DIR
    0xE6 -> typeOf _PTX_DBR      -- Draw B-Axis Rule
    0xE7 -> typeOf _PTX_DBR
    0xEE -> typeOf _PTX_RPS      -- Repeat String
    0xEF -> typeOf _PTX_RPS
    0xF0 -> typeOf _PTX_SCFL     -- Set Coded Font Local
    0xF1 -> typeOf _PTX_SCFL
    0xF2 -> typeOf _PTX_BSU      -- Begin Suppression
    0xF3 -> typeOf _PTX_BSU
    0xF4 -> typeOf _PTX_ESU      -- Begin Suppression
    0xF5 -> typeOf _PTX_ESU
    0xF6 -> typeOf _PTX_STO      -- Set Text Orientation
    0xF7 -> typeOf _PTX_STO
    0xF8 -> typeOf _PTX_NOP      -- No Operation
    0xF9 -> typeOf _PTX_NOP
    0xDA -> typeOf _PTX_TRN      -- Transparent Data
    0xDB -> typeOf _PTX_TRN
    _    -> typeOf _Unknown

-- XXX -- refactor it back to class
get' :: (Binary t, Buf b, Binary b) => ((t, b) -> c) -> BinHandle -> IO c
get' constr bh = do
    buf     <- get bh
    buftype <- getBufType buf
    let chunk = constr (buftype, buf)
    let (pstr, len) = bufToPStrLen buf
    addFinalizer chunk $ touchForeignPtr pstr
    return chunk

getBufType buf = do
    bh <- openBinBuf $ bufToPStrLen buf
    get bh

instance Binary PTX_ where
    put bh (PTX_ (_, buf)) = put bh buf
    get = get' PTX_

instance Binary MCF_ where
    put bh (MCF_ (_, buf)) = put bh buf
    get bh = do
        buf     <- get bh
        return $ MCF_ (N0 (), buf)

instance Binary T_ where
    put bh (T_ (_, buf)) = put bh buf
    get = get' T_
