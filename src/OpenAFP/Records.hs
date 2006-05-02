{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  OpenAFP.Records
-- Copyright   :  (c) Audrey Tang 2004, 2006
-- License     :  BSD-style
-- 
-- Maintainer  :  audreyt@audreyt.org
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
lookupMCF _ _ = chunkTypeOf _MCF_T    -- MCF Triplets

lookupAFP :: AFP_ -> N3 -> ChunkType
lookupAFP _ x = case x of
    0xD38C87 -> chunkTypeOf _CPI      -- Code Page Index
    0xD38C89 -> chunkTypeOf _FNI      -- Font Index
    0xD38C8A -> chunkTypeOf _CFI      -- Coded Font Index
    0xD3A088 -> chunkTypeOf _MFC      -- Medium Finishing Control
    0xD3A090 -> chunkTypeOf _TLE      -- Tag Logical Element
    0xD3A288 -> chunkTypeOf _MCC      -- Medium Copy Count
    0xD3A289 -> chunkTypeOf _FNM      -- Font Patterns Map
    0xD3A66B -> chunkTypeOf _OBD      -- Object Area Descriptor
    0xD3A67B -> chunkTypeOf _IID      -- IM Image Input Descriptor (C)
    0xD3A687 -> chunkTypeOf _CPD      -- Code Page Descriptor
    0xD3A688 -> chunkTypeOf _MDD      -- Medium Descriptor
    0xD3A689 -> chunkTypeOf _FND      -- Font Descriptor
    0xD3A692 -> chunkTypeOf _CDD      -- Container Data Descriptor
    0xD3A69B -> chunkTypeOf _PTD1     -- Presentation Text Descriptor Format-1 (C)
    0xD3A6AF -> chunkTypeOf _PGD      -- Page Descriptor
    0xD3A6BB -> chunkTypeOf _GDD      -- Graphics Data Descriptor
    0xD3A6C5 -> chunkTypeOf _FGD      -- Form Environment Group Descriptor (O)
    0xD3A6E3 -> chunkTypeOf _DXD      -- Data Map Transmission Subcase Descriptor
    0xD3A6E7 -> chunkTypeOf _LND      -- Line Descriptor
    0xD3A6EB -> chunkTypeOf _BDD      -- Bar Code Data Descriptor
    0xD3A6FB -> chunkTypeOf _IDD      -- Image Data Descriptor
    0xD3A77B -> chunkTypeOf _IOC      -- IM Image Output Control (C)
    0xD3A787 -> chunkTypeOf _CPC      -- Code Page Control
    0xD3A788 -> chunkTypeOf _MMC      -- Medium Modification Control
    0xD3A789 -> chunkTypeOf _FNC      -- Font Control
    0xD3A78A -> chunkTypeOf _CFC      -- Coded Font Control
    0xD3A79B -> chunkTypeOf _CTC      -- Composed Text Control (O)
    0xD3A7AF -> chunkTypeOf _PMC      -- Page Modification Control
    0xD3A85F -> chunkTypeOf _BPS      -- Begin Page Segment
    0xD3A877 -> chunkTypeOf _BCA      -- Begin Color Attribute Table
    0xD3A87B -> chunkTypeOf _BII      -- Begin IM Image (C)
    0xD3A887 -> chunkTypeOf _BCP      -- Begin Code Page
    0xD3A889 -> chunkTypeOf _BFN      -- Begin Font
    0xD3A88A -> chunkTypeOf _BCF      -- Begin Coded Font
    0xD3A892 -> chunkTypeOf _BOC      -- Begin Object Container
    0xD3A89B -> chunkTypeOf _BPT      -- Begin Presentation Text Object
    0xD3A8A7 -> chunkTypeOf _BDI      -- Begin Document Index
    0xD3A8A8 -> chunkTypeOf _BDT      -- Begin Document
    0xD3A8AD -> chunkTypeOf _BNG      -- Begin Named Page Group
    0xD3A8AF -> chunkTypeOf _BPG      -- Begin Page
    0xD3A8BB -> chunkTypeOf _BGR      -- Begin Graphics Object
    0xD3A8C4 -> chunkTypeOf _BDG      -- Begin Document Environment Group
    0xD3A8C5 -> chunkTypeOf _BFG      -- Begin Form Environment Group (O)
    0xD3A8C6 -> chunkTypeOf _BRG      -- Begin Resource Group
    0xD3A8C7 -> chunkTypeOf _BOG      -- Begin Object Environment Group
    0xD3A8C9 -> chunkTypeOf _BAG      -- Begin Active Environment Group
    0xD3A8CA -> chunkTypeOf _BDM      -- Begin Data Map
    0xD3A8CB -> chunkTypeOf _BPM      -- Begin Page Map
    0xD3A8CC -> chunkTypeOf _BMM      -- Begin Medium Map
    0xD3A8CD -> chunkTypeOf _BFM      -- Begin Form Map
    0xD3A8CE -> chunkTypeOf _BR       -- Begin Resource (R)
    0xD3A8D9 -> chunkTypeOf _BSG      -- Begin Resource Environment Group
    0xD3A8DF -> chunkTypeOf _BMO      -- Begin Overlay
    0xD3A8E3 -> chunkTypeOf _BDX      -- Begin Data Map Transmission Subcase
    0xD3A8EB -> chunkTypeOf _BBC      -- Begin Bar Code Object
    0xD3A8FB -> chunkTypeOf _BIM      -- Begin Image Object
    0xD3A95F -> chunkTypeOf _EPS      -- End Page Segment
    0xD3A977 -> chunkTypeOf _ECA      -- End Color Attribute Table
    0xD3A97B -> chunkTypeOf _EII      -- End IM Image (C)
    0xD3A987 -> chunkTypeOf _ECP      -- End Code Page
    0xD3A989 -> chunkTypeOf _EFN      -- End Font
    0xD3A98A -> chunkTypeOf _ECF      -- End Coded Font
    0xD3A992 -> chunkTypeOf _EOC      -- End Object Container
    0xD3A99B -> chunkTypeOf _EPT      -- End Presentation Text Object
    0xD3A9A7 -> chunkTypeOf _EDI      -- End Document Index
    0xD3A9A8 -> chunkTypeOf _EDT      -- End Document
    0xD3A9AD -> chunkTypeOf _ENG      -- End Named Page Group
    0xD3A9AF -> chunkTypeOf _EPG      -- End Page
    0xD3A9BB -> chunkTypeOf _EGR      -- End Graphics Object
    0xD3A9C4 -> chunkTypeOf _EDG      -- End Document Environment Group
    0xD3A9C5 -> chunkTypeOf _EFG      -- End Form Environment Group (O)
    0xD3A9C6 -> chunkTypeOf _ERG      -- End Resource Group
    0xD3A9C7 -> chunkTypeOf _EOG      -- End Object Environment Group
    0xD3A9C9 -> chunkTypeOf _EAG      -- End Active Environment Group
    0xD3A9CA -> chunkTypeOf _EDM      -- End Data Map
    0xD3A9CB -> chunkTypeOf _EPM      -- End Page Map
    0xD3A9CC -> chunkTypeOf _EMM      -- End Medium Map
    0xD3A9CD -> chunkTypeOf _EFM      -- End Form Map
    0xD3A9CE -> chunkTypeOf _ER       -- End Resource (R)
    0xD3A9D9 -> chunkTypeOf _ESG      -- End Resource Environment Group
    0xD3A9DF -> chunkTypeOf _EMO      -- End Overlay
    0xD3A9E3 -> chunkTypeOf _EDX      -- End Data Map Transmission Subcase
    0xD3A9EB -> chunkTypeOf _EBC      -- End Bar Code Object
    0xD3A9FB -> chunkTypeOf _EIM      -- End Image Object
    0xD3AAE7 -> chunkTypeOf _LNC      -- Line Descriptor Count
    0xD3AB77 -> chunkTypeOf _MCA      -- Map Color Attribute Table
    0xD3AB88 -> chunkTypeOf _MMT      -- Map Media Type
    0xD3AB89 -> chunkTypeOf _FNN      -- Font Names (Outline Fonts Only)
    0xD3AB8A -> chunkTypeOf _MCF      -- Map Coded Font
    0xD3AB92 -> chunkTypeOf _MCD      -- Map Container Data
    0xD3ABAF -> chunkTypeOf _MPG      -- Map Page
    0xD3ABBB -> chunkTypeOf _MGO      -- Map Graphics Object
    0xD3ABC3 -> chunkTypeOf _MDR      -- Map Data Resource
    0xD3ABCC -> chunkTypeOf _IMM      -- Invoke Medium Map
    0xD3ABD8 -> chunkTypeOf _MPO      -- Map Page Overlay
    0xD3ABEA -> chunkTypeOf _MSU      -- Map Suppression
    0xD3ABEB -> chunkTypeOf _MBC      -- Map Bar Code Object
    0xD3ABFB -> chunkTypeOf _MIO      -- Map Image Object
    0xD3AC6B -> chunkTypeOf _OBP      -- Object Area Position
    0xD3AC7B -> chunkTypeOf _ICP      -- IM Image Cell Position (C)
    0xD3AC89 -> chunkTypeOf _FNP      -- Font Position
    0xD3ACAF -> chunkTypeOf _PGP1     -- Page Position Format-1 (C)
    0xD3AE89 -> chunkTypeOf _FNO      -- Font Orientation
    0xD3AF5F -> chunkTypeOf _IPS      -- Include Page Segment
    0xD3AFAF -> chunkTypeOf _IPG      -- Include Page
    0xD3AFC3 -> chunkTypeOf _IOB      -- Include Object
    0xD3AFD8 -> chunkTypeOf _IPO      -- Include Page Overlay
    0xD3B077 -> chunkTypeOf _CAT      -- Color Attribute Table
    0xD3B15F -> chunkTypeOf _MPS      -- Map Page Segment
    0xD3B18A -> chunkTypeOf _MCF1     -- Map Coded Font Format-1 (C)
    0xD3B19B -> chunkTypeOf _PTD      -- Presentation Text Data Descriptor
    0xD3B1AF -> chunkTypeOf _PGP      -- Page Position
    0xD3B1DF -> chunkTypeOf _MMO      -- Map Medium Overlay
    0xD3B288 -> chunkTypeOf _PFC      -- Presentation Fidelity Control
    0xD3B2A7 -> chunkTypeOf _IEL      -- Index Element
    0xD3B490 -> chunkTypeOf _LLE      -- Link Logical Element
    0xD3EE7B -> chunkTypeOf _IRD      -- IM Image Raster Data (C)
    0xD3EE89 -> chunkTypeOf _FNG      -- Font Patterns
    0xD3EE92 -> chunkTypeOf _OCD      -- Object Container Data
    0xD3EE9B -> chunkTypeOf _PTX      -- Presentation Text Data
    0xD3EEBB -> chunkTypeOf _GAD      -- Graphics Data
    0xD3EEEB -> chunkTypeOf _BDA      -- Bar Code Data
    0xD3EEEE -> chunkTypeOf _NOP      -- No Operation
    0xD3EEFB -> chunkTypeOf _IPD      -- Image Picture Data
    _        -> chunkTypeOf _Unknown


---[ Triplets ]-------------------------------------------------

lookupT :: T_ -> N1 -> ChunkType
lookupT _ x = case x of
    0x01 -> chunkTypeOf _T_CGCSGI    -- Coded Graphic Character Set Global ID
    0x02 -> chunkTypeOf _T_FQN       -- Fully Qualified Name
    0x04 -> chunkTypeOf _T_MO        -- Mapping Option
    0x10 -> chunkTypeOf _T_OCL       -- Object Classification
    0x18 -> chunkTypeOf _T_MIS       -- MO:DCA Interchange Set
    0x1D -> chunkTypeOf _T_TO        -- Text Orientation (R)
    0x1F -> chunkTypeOf _T_FDS       -- Font Descriptor Specification
    0x20 -> chunkTypeOf _T_FCGCSGI   -- Font Coded Graphic Character Set Global Identifier
    0x21 -> chunkTypeOf _T_OFSS      -- Object Function Set Specification
    0x22 -> chunkTypeOf _T_ROT       -- Resource Object Type (R)
    0x23 -> chunkTypeOf _T_ERLI      -- Extended Resource Local ID
    0x24 -> chunkTypeOf _T_RLI       -- Resource Local ID
    0x25 -> chunkTypeOf _T_RSN       -- Resource Section Number
    0x26 -> chunkTypeOf _T_CR        -- Character Rotation
    0x27 -> chunkTypeOf _T_LDOPM     -- Line Data Object Position Migration (R)
    0x2D -> chunkTypeOf _T_OBO       -- Object Byte Offset
    0x36 -> chunkTypeOf _T_AV        -- Attribute Value
    0x43 -> chunkTypeOf _T_DP        -- Descriptor Position
    0x45 -> chunkTypeOf _T_MEC       -- Media Eject Control
    0x46 -> chunkTypeOf _T_POCP      -- Page Overlay Conditional Processing
    0x47 -> chunkTypeOf _T_RUA       -- Resource Usage Attribute
    0x4B -> chunkTypeOf _T_OAMU      -- Object Area Measurement Units
    0x4C -> chunkTypeOf _T_OAS       -- Object Area Size
    0x4D -> chunkTypeOf _T_AD        -- Area Definition
    0x4E -> chunkTypeOf _T_CS        -- Color Specification
    0x50 -> chunkTypeOf _T_ESI       -- Encoding Scheme ID
    0x56 -> chunkTypeOf _T_MMPN      -- Medium Map Page Number
    0x57 -> chunkTypeOf _T_OBE       -- Object Byte Extent
    0x58 -> chunkTypeOf _T_OSFO      -- Object Structured Field Offset
    0x59 -> chunkTypeOf _T_OSFE      -- Object Structured Field Extent
    0x5A -> chunkTypeOf _T_OO        -- Object Offset
    0x5D -> chunkTypeOf _T_FHSF      -- Font Horizontal Scale Factor
    0x5E -> chunkTypeOf _T_OCO       -- Object Count
    0x62 -> chunkTypeOf _T_LDTS      -- Local Date and Time Stamp
    0x63 -> chunkTypeOf _T_OCH       -- Object Checksum (R)
--  0x63 -> chunkTypeOf _T_T1CRMT    -- Type 1 - CRC Resource Management Triplet
--  0x63 -> chunkTypeOf _T_T2FRMT    -- Type 2 - Font Resource Management Triplet
    0x64 -> chunkTypeOf _T_OOI       -- Object Origin Identifier (R)
    0x65 -> chunkTypeOf _T_C         -- Comment
    0x68 -> chunkTypeOf _T_MOR       -- Medium Orientation
    0x6C -> chunkTypeOf _T_ROI       -- Resource Object Include
    0x6D -> chunkTypeOf _T_EF        -- Extension Font
    0x70 -> chunkTypeOf _T_PSRM      -- Presentation Space Reset Mixing
    0x71 -> chunkTypeOf _T_PSMR      -- Presentation Space Mixing Rules
    0x72 -> chunkTypeOf _T_UDTS      -- Universal Date and Time Stamp
    0x73 -> chunkTypeOf _T_II        -- IMM Insertion (R)
    0x74 -> chunkTypeOf _T_TS        -- Toner Saver
    0x75 -> chunkTypeOf _T_CF        -- Color Fidelity
    0x78 -> chunkTypeOf _T_FF        -- Font Fidelity
    0x79 -> chunkTypeOf _T_MA        -- Metric Adjustment
    0x80 -> chunkTypeOf _T_AQ        -- Attribute Qualifier
    0x81 -> chunkTypeOf _T_PPI       -- Page Position Information
    0x82 -> chunkTypeOf _T_PV        -- Parameter Value
    0x83 -> chunkTypeOf _T_PC        -- Presentation Control
    0x84 -> chunkTypeOf _T_FRMT      -- Font Resolution and Metric Technology
    0x85 -> chunkTypeOf _T_FO        -- Finishing Operation
    0x87 -> chunkTypeOf _T_MF        -- Media Fidelity
    _    -> chunkTypeOf _Unknown

---[ PTX ]------------------------------------------------------

lookupPTX :: PTX_ -> N1 -> ChunkType
lookupPTX _ x = case x of
    0x74 -> chunkTypeOf _PTX_STC      -- Set Text Color
    0x75 -> chunkTypeOf _PTX_STC
    0xC0 -> chunkTypeOf _PTX_SIM      -- Set Inline Margin
    0xC1 -> chunkTypeOf _PTX_SIM
    0xC2 -> chunkTypeOf _PTX_SIA      -- Set Intercharacter Adjustment
    0xC3 -> chunkTypeOf _PTX_SIA
    0xC4 -> chunkTypeOf _PTX_SVI      -- Set Variable-Space Character Increment
    0xC5 -> chunkTypeOf _PTX_SVI
    0xC6 -> chunkTypeOf _PTX_AMI      -- Absolute Move Inline
    0xC7 -> chunkTypeOf _PTX_AMI
    0xC8 -> chunkTypeOf _PTX_RMI      -- Relative Move Inline
    0xC9 -> chunkTypeOf _PTX_RMI
    0xD0 -> chunkTypeOf _PTX_SBI      -- Set Baseline Increment
    0xD1 -> chunkTypeOf _PTX_SBI
    0xD2 -> chunkTypeOf _PTX_AMB      -- Absolute Move Baseline
    0xD3 -> chunkTypeOf _PTX_AMB
    0xD4 -> chunkTypeOf _PTX_RMB      -- Relative Move Baseline
    0xD5 -> chunkTypeOf _PTX_RMB
    0xD8 -> chunkTypeOf _PTX_BLN      -- Begin Line Next
    0xD9 -> chunkTypeOf _PTX_BLN
    0xE4 -> chunkTypeOf _PTX_DIR      -- Draw I-Axis Rule
    0xE5 -> chunkTypeOf _PTX_DIR
    0xE6 -> chunkTypeOf _PTX_DBR      -- Draw B-Axis Rule
    0xE7 -> chunkTypeOf _PTX_DBR
    0xEE -> chunkTypeOf _PTX_RPS      -- Repeat String
    0xEF -> chunkTypeOf _PTX_RPS
    0xF0 -> chunkTypeOf _PTX_SCFL     -- Set Coded Font Local
    0xF1 -> chunkTypeOf _PTX_SCFL
    0xF2 -> chunkTypeOf _PTX_BSU      -- Begin Suppression
    0xF3 -> chunkTypeOf _PTX_BSU
    0xF4 -> chunkTypeOf _PTX_ESU      -- Begin Suppression
    0xF5 -> chunkTypeOf _PTX_ESU
    0xF6 -> chunkTypeOf _PTX_STO      -- Set Text Orientation
    0xF7 -> chunkTypeOf _PTX_STO
    0xF8 -> chunkTypeOf _PTX_NOP      -- No Operation
    0xF9 -> chunkTypeOf _PTX_NOP
    0xDA -> chunkTypeOf _PTX_TRN      -- Transparent Data
    0xDB -> chunkTypeOf _PTX_TRN
    _    -> chunkTypeOf _Unknown

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
