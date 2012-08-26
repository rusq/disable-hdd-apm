;*winoictl.h, usb.h, usb200.h, usb100.h, usbioctl.h, usbiodef.h

;BUILD Version: 0016
;Module Name:   winioctl.h
;Abstract:      This module defines the 32-Bit Windows Device I/O control codes.

;GUID_DEVINTERFACE_DISK GUID $53f56307,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_CDROM GUID $53f56308,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_PARTITION GUID $53f5630a,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_TAPE GUID $53f5630b,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_WRITEONCEDISK GUID $53f5630c,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_VOLUME GUID $53f5630d,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_MEDIUMCHANGER GUID $53f56310,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_FLOPPY GUID $53f56311,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_CDCHANGER GUID $53f56312,$b6bf,$11d0,$94,$f2,0,$a0,$c9,$1e,$fb,$8b
;GUID_DEVINTERFACE_STORAGEPORT GUID $2accfe60,$c130,$11d2,$b0,$82,0,$a0,$c9,$1e,$fb,$8b
;WDI_STORAGE_PREDICT_FAILURE_DPS_GUID GUID $e9f2d03a,$747c,$41c2,$bb,$9a,$2,$c6,$2b,$6d,$5f,$cb
;GUID_DEVINTERFACE_HIDDEN_VOLUME GUID $7f108a28,$9833,$4b3b,$b7,$80,$2c,$6b,$5f,$a5,$c0,$62
;GUID_DEVINTERFACE_COMPORT GUID $86e0d1e0,$8089,$11d0,$9c,$e4,$8,0,$3e,$30,$1f,$73
;GUID_DEVINTERFACE_SERENUM_BUS_ENUMERATOR GUID $4D36E978,$E325,$11CE,$BF,$C1,$8,0,$2B,$E1,$3,$18

FILE_DEVICE_BEEP = 1
FILE_DEVICE_CD_ROM = 2
FILE_DEVICE_CD_ROM_FILE_SYSTEM = 3
FILE_DEVICE_CONTROLLER = 4
FILE_DEVICE_DATALINK = 5
FILE_DEVICE_DFS = 6
FILE_DEVICE_DISK = 7
FILE_DEVICE_DISK_FILE_SYSTEM = 8
FILE_DEVICE_FILE_SYSTEM = 9
FILE_DEVICE_INPORT_PORT = $a
FILE_DEVICE_KEYBOARD = $b
FILE_DEVICE_MAILSLOT = $c
FILE_DEVICE_MIDI_IN = $d
FILE_DEVICE_MIDI_OUT = $e
FILE_DEVICE_MOUSE = $f
FILE_DEVICE_MULTI_UNC_PROVIDER = $10
FILE_DEVICE_NAMED_PIPE = $11
FILE_DEVICE_NETWORK = $12
FILE_DEVICE_NETWORK_BROWSER = $13
FILE_DEVICE_NETWORK_FILE_SYSTEM = $14
FILE_DEVICE_NULL = $15
FILE_DEVICE_PARALLEL_PORT = $16
FILE_DEVICE_PHYSICAL_NETCARD = $17
FILE_DEVICE_PRINTER = $18
FILE_DEVICE_SCANNER = $19
FILE_DEVICE_SERIAL_MOUSE_PORT = $1a
FILE_DEVICE_SERIAL_PORT = $1b
FILE_DEVICE_SCREEN = $1c
FILE_DEVICE_SOUND = $1d
FILE_DEVICE_STREAMS = $1e
FILE_DEVICE_TAPE = $1f
FILE_DEVICE_TAPE_FILE_SYSTEM = $20
FILE_DEVICE_TRANSPORT = $21
FILE_DEVICE_UNKNOWN = $22
FILE_DEVICE_VIDEO = $23
FILE_DEVICE_VIRTUAL_DISK = $24
FILE_DEVICE_WAVE_IN = $25
FILE_DEVICE_WAVE_OUT = $26
FILE_DEVICE_8042_PORT = $27
FILE_DEVICE_NETWORK_REDIRECTOR = $28
FILE_DEVICE_BATTERY = $29
FILE_DEVICE_BUS_EXTENDER = $2a
FILE_DEVICE_MODEM = $2b
FILE_DEVICE_VDM = $2c
FILE_DEVICE_MASS_STORAGE = $2d
FILE_DEVICE_SMB = $2e
FILE_DEVICE_KS = $2f
FILE_DEVICE_CHANGER = $30
FILE_DEVICE_SMARTCARD = $31
FILE_DEVICE_ACPI = $32
FILE_DEVICE_DVD = $33
FILE_DEVICE_FULLSCREEN_VIDEO = $34
FILE_DEVICE_DFS_FILE_SYSTEM = $35
FILE_DEVICE_DFS_VOLUME = $36
FILE_DEVICE_SERENUM = $37
FILE_DEVICE_TERMSRV = $38
FILE_DEVICE_KSEC = $39
FILE_DEVICE_FIPS = $3A
FILE_DEVICE_INFINIBAND = $3B
FILE_DEVICE_VMBUS = $3E
FILE_DEVICE_CRYPT_PROVIDER = $3F
FILE_DEVICE_WPD = $40
FILE_DEVICE_BLUETOOTH = $41
FILE_DEVICE_MT_COMPOSITE = $42
FILE_DEVICE_MT_TRANSPORT = $43
FILE_DEVICE_BIOMETRIC = $44
FILE_DEVICE_PMI = $45

METHOD_BUFFERED = 0
METHOD_IN_DIRECT = 1
METHOD_OUT_DIRECT = 2
METHOD_NEITHER = 3

METHOD_DIRECT_TO_HARDWARE = METHOD_IN_DIRECT
METHOD_DIRECT_FROM_HARDWARE = METHOD_OUT_DIRECT

FILE_ANY_ACCESS = 0
FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS
FILE_READ_ACCESS = 1
FILE_WRITE_ACCESS = 2

IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE

IOCTL_STORAGE_CHECK_VERIFY = $2D4800
IOCTL_STORAGE_CHECK_VERIFY2 = $2D0800
IOCTL_STORAGE_MEDIA_REMOVAL = $2D4804
IOCTL_STORAGE_EJECT_MEDIA = $2D4808
IOCTL_STORAGE_LOAD_MEDIA = $2D480C
IOCTL_STORAGE_LOAD_MEDIA2 = $2D080C
IOCTL_STORAGE_RESERVE = $2D4810
IOCTL_STORAGE_RELEASE = $2D4814
IOCTL_STORAGE_FIND_NEW_DEVICES = $2D4818
IOCTL_STORAGE_EJECTION_CONTROL = $2D0940
IOCTL_STORAGE_MCN_CONTROL = $2D0944
IOCTL_STORAGE_GET_MEDIA_TYPES = $2D0C00
IOCTL_STORAGE_GET_MEDIA_TYPES_EX = $2D0C04
IOCTL_STORAGE_GET_MEDIA_SERIAL_NUMBER = $2D0C10
IOCTL_STORAGE_GET_HOTPLUG_INFO = $2D0C14
IOCTL_STORAGE_SET_HOTPLUG_INFO = $2DCC18
IOCTL_STORAGE_RESET_BUS = $2D5000
IOCTL_STORAGE_RESET_DEVICE = $2D5004
IOCTL_STORAGE_BREAK_RESERVATION = $2D5014
IOCTL_STORAGE_PERSISTENT_RESERVE_IN = $2D5018
IOCTL_STORAGE_PERSISTENT_RESERVE_OUT = $2DD01C
IOCTL_STORAGE_GET_DEVICE_NUMBER = $2D1080
IOCTL_STORAGE_PREDICT_FAILURE = $2D1100
IOCTL_STORAGE_READ_CAPACITY = $2D5140

IOCTL_STORAGE_QUERY_PROPERTY = $2D1400
IOCTL_STORAGE_MANAGE_DATA_SET_ATTRIBUTES = $2D9404
IOCTL_STORAGE_GET_BC_PROPERTIES = $2D5800
IOCTL_STORAGE_ALLOCATE_BC_STREAM = $2DD804
IOCTL_STORAGE_FREE_BC_STREAM = $2DD808

IOCTL_STORAGE_CHECK_PRIORITY_HINT_SUPPORT = $2D1880
OBSOLETE_IOCTL_STORAGE_RESET_BUS = $2DD000
OBSOLETE_IOCTL_STORAGE_RESET_DEVICE = $2DD004

struct STORAGE_HOTPLUG_INFO
       Size dd ?
       MediaRemovable db ?
       MediaHotplug db ?
       DeviceHotplug db ?
       WriteCacheEnableOverride db ?
ends

struct STORAGE_DEVICE_NUMBER
       DeviceType dd ?
       DeviceNumber dd ?
       PartitionNumber dd ?
ends

struct STORAGE_BUS_RESET_REQUEST
       PathId db ?
ends

struct STORAGE_BREAK_RESERVATION_REQUEST
       Length dd ?
       unused db ?
       PathId db ?
       TargetId db ?
       Lun db ?
ends

struct PREVENT_MEDIA_REMOVAL
       PreventMediaRemoval db ?
ends

struct CLASS_MEDIA_CHANGE_CONTEXT
       MediaChangeCount dd ?
       NewState dd ?
ends

struct TAPE_STATISTICS
       Version dd ?
       Flags dd ?
       RecoveredWrites dq ?
       UnrecoveredWrites dq ?
       RecoveredReads dq ?
       UnrecoveredReads dq ?
       CompressionRatioReads db ?
       CompressionRatioWrites db ?,?,?,?,?,?,?
ends

RECOVERED_WRITES_VALID = 1
UNRECOVERED_WRITES_VALID = 2
RECOVERED_READS_VALID = 4
UNRECOVERED_READS_VALID = 8
WRITE_COMPRESSION_INFO_VALID = $10
READ_COMPRESSION_INFO_VALID = $20

struct TAPE_GET_STATISTICS
       Operation dd ?
ends

TAPE_RETURN_STATISTICS = 0
TAPE_RETURN_ENV_INFO = 1
TAPE_RESET_STATISTICS = 2

DDS_4mm = $20
MiniQic = $21
Travan = $22
QIC = $23
MP_8mm = $24
AME_8mm = $25
AIT1_8mm = $26
DLT = $27
NCTP = $28
IBM_3480 = $29
IBM_3490E = $2a
IBM_Magstar_3590 = $2b
IBM_Magstar_MP = $2c
STK_DATA_D3 = $2d
SONY_DTF = $2e
DV_6mm = $2f
DMI = $30
SONY_D2 = $31
CLEANER_CARTRIDGE = $32
CD_ROM = $33
CD_R = $34
CD_RW = $35
DVD_ROM = $36
DVD_R = $37
DVD_RW = $38
MO_3_RW = $39
MO_5_WO = $3a
MO_5_RW = $3b
MO_5_LIMDOW = $3c
PC_5_WO = $3d
PC_5_RW = $3e
PD_5_RW = $3f
ABL_5_WO = $40
PINNACLE_APEX_5_RW = $41
SONY_12_WO = $42
PHILIPS_12_WO = $43
HITACHI_12_WO = $44
CYGNET_12_WO = $45
KODAK_14_WO = $46
MO_NFR_525 = $47
NIKON_12_RW = $48
IOMEGA_ZIP = $49
IOMEGA_JAZ = $4a
SYQUEST_EZ135 = $4b
SYQUEST_EZFLYER = $4c
SYQUEST_SYJET = $4d
AVATAR_F2 = $4e
MP2_8mm = $4f
DST_S = $50
DST_M = $51
DST_L = $52
VXATape_1 = $53
VXATape_2 = $54

STK_9840 = $55
LTO_Ultrium = $56
LTO_Accelis = $57
DVD_RAM = $58
AIT_8mm = $59
ADR_1 = $5a
ADR_2 = $5b
STK_9940 = $5c
SAIT = $5d
VXATape = $5e

MEDIA_ERASEABLE = 1
MEDIA_WRITE_ONCE = 2
MEDIA_READ_ONLY = 4
MEDIA_READ_WRITE = 8
MEDIA_WRITE_PROTECTED = $100
MEDIA_CURRENTLY_MOUNTED = $80000000

BusTypeUnknown = 0
BusTypeScsi = 1
BusTypeAtapi = 2
BusTypeAta = 3
BusType1394 = 4
BusTypeSsa = 5
BusTypeFibre = 6
BusTypeUsb = 7
BusTypeRAID = 8
BusTypeiScsi = 9
BusTypeSas = 10
BusTypeSata = 11
BusTypeSd = 12
BusTypeMmc = 13
BusTypeVirtual = 14
BusTypeFileBackedVirtual = 15
BusTypeMax = 16
BusTypeMaxReserved = $7F

struct DEVICE_MEDIA_INFO
       union
	  struct
	      Cylinders dq ?
	      MediaType dd ?
	      TracksPerCylinder dd ?
	      SectorsPerTrack dd ?
	      BytesPerSector dd ?
	      NumberMediaSides dd ?
	      MediaCharacteristics dd ?
	  ends

	  struct
	      rdi_Cylinders dq ?
	      rdi_MediaType dd ?
	      rdi_TracksPerCylinder dd ?
	      rdi_SectorsPerTrack dd ?
	      rdi_BytesPerSector dd ?
	      rdi_NumberMediaSides dd ?
	      rdi_MediaCharacteristics dd ?
	  ends

	  struct
	      ti_MediaType dd ?
	      ti_MediaCharacteristics dd ?
	      ti_CurrentBlockSize dd ?
	      ti_BusType dd ?
	      union
		  struct
		      MediumType db ?
		      DensityCode db ?
		  ends
	      ends
	  ends
      ends
ends

struct GET_MEDIA_TYPES
       DeviceType dd ?
       MediaInfoCount dd ?
       MediaInfo DEVICE_MEDIA_INFO
ends

struct STORAGE_PREDICT_FAILURE
       PredictFailure dd ?
       VendorSpecific db 512 dup(?)
ends

PropertyStandardQuery = 0
PropertyExistsQuery = 1
PropertyMaskQuery = 2
PropertyQueryMaxDefined = 3

StorageDeviceProperty = 0
StorageAdapterProperty = 1
StorageDeviceIdProperty = 2
StorageDeviceUniqueIdProperty = 3
StorageDeviceWriteCacheProperty = 4
StorageMiniportProperty = 5
StorageAccessAlignmentProperty = 6
StorageDeviceSeekPenaltyProperty = 7
StorageDeviceTrimProperty = 8
StorageDeviceWriteAggregationProperty = 9

struct STORAGE_PROPERTY_QUERY
       PropertyId dd ?
       QueryType dd ?
       AdditionalParameters db ?,?,?,?
ends

struct STORAGE_DESCRIPTOR_HEADER
       Version dd ?
       Size dd ?
ends

struct STORAGE_DEVICE_DESCRIPTOR
       Version dd ?
       Size dd ?
       DeviceType db ?
       DeviceTypeModifier db ?
       RemovableMedia db ?
       CommandQueueing db ?
       VendorIdOffset dd ?
       ProductIdOffset dd ?
       ProductRevisionOffset dd ?
       SerialNumberOffset dd ?
       BusType dd ?
       RawPropertiesLength dd ?
       RawDeviceProperties db ?,?,?,?
ends

struct STORAGE_ADAPTER_DESCRIPTOR
       Version dd ?
       Size dd ?
       MaximumTransferLength dd ?
       MaximumPhysicalPages dd ?
       AlignmentMask dd ?
       AdapterUsesPio db ?
       AdapterScansDown db ?
       CommandQueueing db ?
       AcceleratedTransfer db ?
       BusType db ?,?
       BusMajorVersion dw ?
       BusMinorVersion dw ?,?
ends

struct STORAGE_ACCESS_ALIGNMENT_DESCRIPTOR
       Version dd ?
       Size dd ?
       BytesPerCacheLine dd ?
       BytesOffsetForCacheAlignment dd ?
       BytesPerLogicalSector dd ?
       BytesPerPhysicalSector dd ?
       BytesOffsetForSectorAlignment dd ?
ends

StoragePortCodeSetReserved = 0
StoragePortCodeSetStorport = 1
StoragePortCodeSetSCSIport = 2

struct STORAGE_MINIPORT_DESCRIPTOR
       Version dd ?
       Size dd ?
       Portdriver dd ?
       LUNResetSupported db ?
       TargetResetSupported db ?,?,?
ends

StorageIdCodeSetReserved = 0
StorageIdCodeSetBinary = 1
StorageIdCodeSetAscii = 2
StorageIdCodeSetUtf8 = 3

StorageIdTypeVendorSpecific = 0
StorageIdTypeVendorId = 1
StorageIdTypeEUI64 = 2
StorageIdTypeFCPHName = 3
StorageIdTypePortRelative = 4
StorageIdTypeTargetPortGroup = 5
StorageIdTypeLogicalUnitGroup = 6
StorageIdTypeMD5LogicalUnitIdentifier = 7
StorageIdTypeScsiNameString = 8

StorageIdTypeNAA = StorageIdTypeFCPHName

StorageIdNAAFormatIEEEExtended = 2
StorageIdNAAFormatIEEERegistered = 3
StorageIdNAAFormatIEEEERegisteredExtended = 5

StorageIdAssocDevice = 0
StorageIdAssocPort = 1
StorageIdAssocTarget = 2

struct STORAGE_IDENTIFIER
       CodeSet dd ?
       Type dd ?
       IdentifierSize dw ?
       NextOffset dw ?
       Association dd ?
       Identifier db ?,?,?,?
ends

struct STORAGE_DEVICE_ID_DESCRIPTOR
       Version dd ?
       Size dd ?
       NumberOfIdentifiers dd ?
       Identifiers db ?,?,?,?
ends

struct DEVICE_SEEK_PENALTY_DESCRIPTOR
       Version dd ?
       Size dd ?
       IncursSeekPenalty db ?,?,?,?
ends

struct DEVICE_WRITE_AGGREGATION_DESCRIPTOR
       Version dd ?
       Size dd ?
       BenefitsFromWriteAggregation db ?,?,?,?
ends

struct DEVICE_TRIM_DESCRIPTOR
       Version dd ?
       Size dd ?
       TrimEnabled db ?,?,?,?
ends

DeviceDsmActionFlag_NonDestructive = $80000000

DeviceDsmAction_None = 0
DeviceDsmAction_Trim = 1
DeviceDsmAction_Notification = (2 or DeviceDsmActionFlag_NonDestructive)

DEVICE_DSM_FLAG_ENTIRE_DATA_SET_RANGE = 1

struct DEVICE_DATA_SET_RANGE
       StartingOffset dq ?
       LengthInBytes dq ?
ends

struct DEVICE_MANAGE_DATA_SET_ATTRIBUTES
       Size dd ?
       Action dd ?
       Flags dd ?
       ParameterBlockOffset dd ?
       ParameterBlockLength dd ?
       DataSetRangesOffset dd ?
       DataSetRangesLength dd ?
ends

struct DEVICE_DSM_NOTIFICATION_PARAMETERS
       Size dd ?
       Flags dd ?
       NumFileTypeIDs dd ?
       FileTypeID GUID
ends

DEVICE_DSM_NOTIFY_FLAG_BEGIN = 1
DEVICE_DSM_NOTIFY_FLAG_END = 2

struct STORAGE_GET_BC_PROPERTIES_OUTPUT
       MaximumRequestsPerPeriod dd ?
       MinimumPeriod dd ?
       MaximumRequestSize dq ?
       EstimatedTimePerRequest dd ?
       NumOutStandingRequests dd ?
       RequestSize dq ?
ends

IOCTL_STORAGE_BC_VERSION = 1

struct STORAGE_ALLOCATE_BC_STREAM_INPUT
       Version dd ?
       RequestsPerPeriod dd ?
       Period dd ?
       RetryFailures db ?
       Discardable db ?
       Reserved1 db ?,?
       AccessType dd ?
       AccessMode dd ?
ends

struct STORAGE_ALLOCATE_BC_STREAM_OUTPUT
       RequestSize dq ?
       NumOutStandingRequests dd ?,?
ends

STORAGE_PRIORITY_HINT_SUPPORTED = 1

struct STORAGE_PRIORITY_HINT_SUPPORT
       SupportFlags dd ?
ends

struct STORAGE_MEDIA_SERIAL_NUMBER_DATA
       Reserved dw ?
       SerialNumberLength dw ?
       SerialNumber = $
ends

struct STORAGE_READ_CAPACITY
       Version dd ?
       Size dd ?
       BlockLength dd ?,?
       NumberOfBlocks dq ?
       DiskLength dq ?
ends

WriteCacheTypeUnknown = 0
WriteCacheTypeNone = 1
WriteCacheTypeWriteBack = 2
WriteCacheTypeWriteThrough = 3

WriteCacheEnableUnknown = 0
WriteCacheDisabled = 1
WriteCacheEnabled = 2

WriteCacheChangeUnknown = 0
WriteCacheNotChangeable = 1
WriteCacheChangeable = 2

WriteThroughUnknown = 0
WriteThroughNotSupported = 1
WriteThroughSupported = 2

struct STORAGE_WRITE_CACHE_PROPERTY
       Version dd ?
       Size dd ?
       WriteCacheType dd ?
       WriteCacheEnabled dd ?
       WriteCacheChangeable dd ?
       WriteThroughSupported dd ?
       FlushCacheSupported db ?
       UserDefinedPowerProtection db ?
       NVCacheEnabled db ?,?
ends

struct PERSISTENT_RESERVE_COMMAND
       Version dd ?
       Size dd ?
       union
	   struct
	       PRIN_ServiceAction db ?
	       PRIN_Reserved1 db ?
	       PRIN_AllocationLength dw ?
	   ends
	   struct
	       PROUT_ServiceAction db ?
	       PROUT_Reserved1 db ?
	       PROUT_Type db ?
	       PROUT_Scope db ?
	       PROUT_ParameterList = $
	   ends
       ends
ends

IOCTL_DISK_BASE = FILE_DEVICE_DISK
IOCTL_DISK_GET_DRIVE_GEOMETRY = $70000
IOCTL_DISK_GET_PARTITION_INFO = $74004
IOCTL_DISK_SET_PARTITION_INFO = $7C008
IOCTL_DISK_GET_DRIVE_LAYOUT = $7400C
IOCTL_DISK_SET_DRIVE_LAYOUT = $7C010
IOCTL_DISK_VERIFY = $70014
IOCTL_DISK_FORMAT_TRACKS = $7C018
IOCTL_DISK_REASSIGN_BLOCKS = $7C01C
IOCTL_DISK_PERFORMANCE = $70020
IOCTL_DISK_IS_WRITABLE = $70024
IOCTL_DISK_LOGGING = $70028
IOCTL_DISK_FORMAT_TRACKS_EX = $7C02C
IOCTL_DISK_HISTOGRAM_STRUCTURE = $70030
IOCTL_DISK_HISTOGRAM_DATA = $70034
IOCTL_DISK_HISTOGRAM_RESET = $70038
IOCTL_DISK_REQUEST_STRUCTURE = $7003C
IOCTL_DISK_REQUEST_DATA = $70040
IOCTL_DISK_PERFORMANCE_OFF = $70060
IOCTL_DISK_CONTROLLER_NUMBER = $70044

SMART_GET_VERSION = $74080
SMART_SEND_DRIVE_COMMAND = $7C084
SMART_RCV_DRIVE_DATA = $7C088

IOCTL_DISK_GET_PARTITION_INFO_EX = $70048
IOCTL_DISK_SET_PARTITION_INFO_EX = $7C04C
IOCTL_DISK_GET_DRIVE_LAYOUT_EX = $70050
IOCTL_DISK_SET_DRIVE_LAYOUT_EX = $7C054
IOCTL_DISK_CREATE_DISK = $7C058
IOCTL_DISK_GET_LENGTH_INFO = $7405C
IOCTL_DISK_GET_DRIVE_GEOMETRY_EX = $700A0
IOCTL_DISK_REASSIGN_BLOCKS_EX = $7C0A4
IOCTL_DISK_UPDATE_DRIVE_SIZE = $7C0C8
IOCTL_DISK_GROW_PARTITION = $7C0D0
IOCTL_DISK_GET_CACHE_INFORMATION = $740D4
IOCTL_DISK_SET_CACHE_INFORMATION = $7C0D8
OBSOLETE_DISK_GET_WRITE_CACHE_STATE = $740DC
IOCTL_DISK_DELETE_DRIVE_LAYOUT = $7C100

IOCTL_DISK_UPDATE_PROPERTIES = $70140

IOCTL_DISK_FORMAT_DRIVE = $7C3CC
IOCTL_DISK_SENSE_DEVICE = $703E0

IOCTL_DISK_CHECK_VERIFY = $74800
IOCTL_DISK_MEDIA_REMOVAL = $74804
IOCTL_DISK_EJECT_MEDIA = $74808
IOCTL_DISK_LOAD_MEDIA = $7480C
IOCTL_DISK_RESERVE = $74810
IOCTL_DISK_RELEASE = $74814
IOCTL_DISK_FIND_NEW_DEVICES = $74818
IOCTL_DISK_GET_MEDIA_TYPES = $70C00

PARTITION_ENTRY_UNUSED = 0
PARTITION_FAT_12 = 1
PARTITION_XENIX_1 = 2
PARTITION_XENIX_2 = 3
PARTITION_FAT_16 = 4
PARTITION_EXTENDED = 5
PARTITION_HUGE = 6
PARTITION_IFS = 7
PARTITION_OS2BOOTMGR = $A
PARTITION_FAT32 = $B
PARTITION_FAT32_XINT13 = $C
PARTITION_XINT13 = $E
PARTITION_XINT13_EXTENDED = $F
PARTITION_PREP = $41
PARTITION_LDM = $42
PARTITION_UNIX = $63
VALID_NTFT = $C0

PARTITION_NTFT = $80

Unknown = 0
F5_1Pt2_512 = 1
F3_1Pt44_512 = 2
F3_2Pt88_512 = 3
F3_20Pt8_512 = 4
F3_720_512 = 5
F5_360_512 = 6
F5_320_512 = 7
F5_320_1024 = 8
F5_180_512 = 9
F5_160_512 = 10
RemovableMedia = 11
FixedMedia = 12
F3_120M_512 = 13
F3_640_512 = 14
F5_640_512 = 15
F5_720_512 = 16
F3_1Pt2_512 = 17
F3_1Pt23_1024 = 18
F5_1Pt23_1024 = 19
F3_128Mb_512 = 20
F3_230Mb_512 = 21
F8_256_128 = 22
F3_200Mb_512 = 23
F3_240M_512 = 24
F3_32M_512 = 25

struct FORMAT_PARAMETERS
       MediaType dd ?
       StartCylinderNumber dd ?
       EndCylinderNumber dd ?
       StartHeadNumber dd ?
       EndHeadNumber dd ?
ends

struct FORMAT_EX_PARAMETERS
       MediaType dd ?
       StartCylinderNumber dd ?
       EndCylinderNumber dd ?
       StartHeadNumber dd ?
       EndHeadNumber dd ?
       FormatGapLength dw ?
       SectorsPerTrack dw ?
       SectorNumber dw ?,?
ends

struct DISK_GEOMETRY
       Cylinders dq ?
       MediaType dd ?
       TracksPerCylinder dd ?
       SectorsPerTrack dd ?
       BytesPerSector dd ?
ends

;WMI_DISK_GEOMETRY_GUID GUID $25007f51,$57c2,$11d1,<$a5,$28,0,$a0,$c9,6,$29,$10>

struct PARTITION_INFORMATION
       StartingOffset dq ?
       PartitionLength dq ?
       HiddenSectors dd ?
       PartitionNumber dd ?
       PartitionType db ?
       BootIndicator db ?
       RecognizedPartition db ?
       RewritePartition db ?,?,?,?,?
ends

struct SET_PARTITION_INFORMATION
       PartitionType db ?
ends

struct DRIVE_LAYOUT_INFORMATION
       PartitionCount dd ?
       Signature dd ?
       PartitionEntry PARTITION_INFORMATION
ends

struct VERIFY_INFORMATION
       StartingOffset dq ?
       Length dd ?,?
ends

struct REASSIGN_BLOCKS
       Reserved dw ?
       Count dw ?
       BlockNumber dd ?
ends

struct REASSIGN_BLOCKS_EX
       Reserved dw ?
       Count dw ?
       BlockNumber dq ?
ends

PARTITION_STYLE_MBR = 0
PARTITION_STYLE_GPT = 1
PARTITION_STYLE_RAW = 2

struct PARTITION_INFORMATION_GPT
       PartitionType GUID
       PartitionId GUID
       Attributes dq ?
       Name du 36 dup(?)
ends

GPT_ATTRIBUTE_PLATFORM_REQUIRED = 1

GPT_BASIC_DATA_ATTRIBUTE_NO_DRIVE_LETTER = $8000000000000000
GPT_BASIC_DATA_ATTRIBUTE_HIDDEN = $4000000000000000
GPT_BASIC_DATA_ATTRIBUTE_SHADOW_COPY = $2000000000000000
GPT_BASIC_DATA_ATTRIBUTE_READ_ONLY = $1000000000000000

struct PARTITION_INFORMATION_MBR
       PartitionType db ?
       BootIndicator db ?
       RecognizedPartition db ?,?
       HiddenSectors dd ?
ends

struct SET_PARTITION_INFORMATION_EX
       PartitionStyle dd ?,?
       union
	   Mbr SET_PARTITION_INFORMATION
	   Gpt PARTITION_INFORMATION_GPT
       ends
ends

struct CREATE_DISK_GPT
       DiskId GUID
       MaxPartitionCount dd ?
ends

struct CREATE_DISK_MBR
       Signature dd ?
ends

struct CREATE_DISK
       PartitionStyle dd ?
       union
	   Mbr	CREATE_DISK_MBR
	   Gpt	CREATE_DISK_GPT
       ends
ends

struct GET_LENGTH_INFORMATION
       Length dq ?
ends

struct PARTITION_INFORMATION_EX
       PartitionStyle dd ?,?
       StartingOffset dq ?
       PartitionLength dq ?
       PartitionNumber dd ?
       RewritePartition db ?,?,?,?
       union
	   Mbr PARTITION_INFORMATION_MBR
	   Gpt PARTITION_INFORMATION_GPT
       ends
ends

struct DRIVE_LAYOUT_INFORMATION_GPT
       DiskId GUID
       StartingUsableOffset dq ?
       UsableLength dq ?
       MaxPartitionCount dd ?,?
ends

struct DRIVE_LAYOUT_INFORMATION_MBR
       Signature dd ?
ends

struct DRIVE_LAYOUT_INFORMATION_EX
       PartitionStyle dd ?
       PartitionCount dd ?
       union
	   Mbr DRIVE_LAYOUT_INFORMATION_MBR
	   Gpt DRIVE_LAYOUT_INFORMATION_GPT
       ends
       PartitionEntry PARTITION_INFORMATION_EX
ends

DetectNone = 0
DetectInt13 = 1
DetectExInt13 = 2

struct DISK_INT13_INFO
       DriveSelect dw ?,?
       MaxCylinders dd ?
       SectorsPerTrack dw ?
       MaxHeads dw ?
       NumberDrives dw ?,?
ends

struct DISK_EX_INT13_INFO
       ExBufferSize dw ?
       ExFlags dw ?
       ExCylinders dd ?
       ExHeads dd ?
       ExSectorsPerTrack dd ?
       ExSectorsPerDrive dq ?
       ExSectorSize dw ?
       ExReserved dw ?,?,?
ends

struct DISK_DETECTION_INFO
       SizeOfDetectInfo dd ?
       DetectionType dd ?
       union
	   struct
	       Int13 DISK_INT13_INFO
	       ExInt13 DISK_EX_INT13_INFO
	   ends
       ends
ends

struct DISK_PARTITION_INFO
       SizeOfPartitionInfo dd ?
       PartitionStyle dd ?
       union
	       struct
		   Signature dd ?
		   CheckSum dd ?
	       ends
	       struct
		   DiskId GUID
	       ends
       ends
ends

struct DISK_GEOMETRY_EX
       Geometry DISK_GEOMETRY
       DiskSize dq ?
       Data db ?,?,?,?,?,?,?,?
ends

struct DISK_CONTROLLER_NUMBER
       ControllerNumber dd ?
       DiskNumber dd ?
ends

EqualPriority = 0
KeepPrefetchedData = 1
KeepReadData = 2

DiskWriteCacheNormal = 0
DiskWriteCacheForceDisable = 1
DiskWriteCacheDisableNotSupported = 2

struct DISK_CACHE_INFORMATION
       ParametersSavable db ?
       ReadCacheEnabled db ?
       WriteCacheEnabled db ?,?
       ReadRetentionPriority dd ?
       WriteRetentionPriority dd ?
       DisablePrefetchTransferLength dw ?
       PrefetchScalar db ?,?
       union
	   struct
	       Minimum dw ?
	       Maximum dw ?
	       MaximumBlocks dw ?,?
	   ends
	   struct
	       bpMinimum dw ?
	       bpMaximum dw ?
	   ends
       ends
ends

struct DISK_GROW_PARTITION
       PartitionNumber dd ?,?
       BytesToGrow dq ?
ends

HIST_NO_OF_BUCKETS = 24

struct HISTOGRAM_BUCKET
       Reads dd ?
       Writes dd ?
ends

HISTOGRAM_BUCKET_SIZE = sizeof.HISTOGRAM_BUCKET

struct DISK_HISTOGRAM
       DiskSize dq ?
       Start dq ?
       End dq ?
       Average dq ?
       AverageRead dq ?
       AverageWrite dq ?
       Granularity dd ?
       Size dd ?
       ReadCount dd ?
       WriteCount dd ?
       Histogram dd ?,?
ends

DISK_HISTOGRAM_SIZE = sizeof.DISK_HISTOGRAM

struct DISK_PERFORMANCE
       BytesRead dq ?
       BytesWritten dq ?
       ReadTime dq ?
       WriteTime dq ?
       IdleTime dq ?
       ReadCount dd ?
       WriteCount dd ?
       QueueDepth dd ?
       SplitCount dd ?
       QueryTime dq ?
       StorageDeviceNumber dd ?
       StorageManagerName dw 10 dup(?)
ends

struct DISK_RECORD
       ByteOffset dq ?
       StartTime dq ?
       EndTime dq ?
       VirtualAddress dd ?
       NumberOfBytes dd ?
       DeviceNumber db ?
       ReadRequest db 7 dup(?)
ends

struct DISK_LOGGING
       Function db ?,?,?,?
       BufferAddress dd ?
       BufferSize dd ?
ends

DISK_LOGGING_START = 0
DISK_LOGGING_STOP = 1
DISK_LOGGING_DUMP = 2
DISK_BINNING = 3

RequestSize = 0
RequestLocation = 1

struct BIN_RANGE
       StartValue dq ?
       Length dq ?
ends

struct PERF_BIN
       NumberOfBins dd ?
       TypeOfBin dd ?
       BinsRanges BIN_RANGE
ends

struct BIN_COUNT
       BinRange BIN_RANGE
       BinCount dd ?,?
ends

struct BIN_RESULTS
       NumberOfBins dd ?,?
       BinCounts BIN_COUNT
ends

struct GETVERSIONINPARAMS
       bVersion db ?
       bRevision db ?
       bReserved db ?
       bIDEDeviceMap db ?
       fCapabilities dd ?
       dwReserved dd ?,?,?,?
ends

CAP_ATA_ID_CMD = 1
CAP_ATAPI_ID_CMD = 2
CAP_SMART_CMD = 4

struct IDEREGS
       bFeaturesReg db ?
       bSectorCountReg db ?
       bSectorNumberReg db ?
       bCylLowReg db ?
       bCylHighReg db ?
       bDriveHeadReg db ?
       bCommandReg db ?
       bReserved db ?
ends

ATAPI_ID_CMD = $A1
ID_CMD = $EC
SMART_CMD = $B0
SMART_CYL_LOW = $4F
SMART_CYL_HI = $C2

struct SENDCMDINPARAMS
       cBufferSize dd ?
       irDriveRegs IDEREGS
       bDriveNumber db ?
       bReserved db ?,?,?
       dwReserved  dd ?,?,?,?
       bBuffer db ?
ends

struct DRIVERSTATUS
       bDriverError db ?
       bIDEError db ?
       bReserved db ?,?
       dwReserved dd ?,?
ends

SMART_NO_ERROR = 0
SMART_IDE_ERROR = 1
SMART_INVALID_FLAG = 2
SMART_INVALID_COMMAND = 3
SMART_INVALID_BUFFER = 4
SMART_INVALID_DRIVE = 5
SMART_INVALID_IOCTL = 6
SMART_ERROR_NO_MEM = 7
SMART_INVALID_REGISTER = 8
SMART_NOT_SUPPORTED = 9
SMART_NO_IDE_DEVICE = 10

SMART_OFFLINE_ROUTINE_OFFLINE = 0
SMART_SHORT_SELFTEST_OFFLINE = 1
SMART_EXTENDED_SELFTEST_OFFLINE = 2
SMART_ABORT_OFFLINE_SELFTEST = 127
SMART_SHORT_SELFTEST_CAPTIVE = 129
SMART_EXTENDED_SELFTEST_CAPTIVE = 130

struct SENDCMDOUTPARAMS
       cBufferSize dd ?
       DriverStatus DRIVERSTATUS
       bBuffer db ?
ends

READ_ATTRIBUTE_BUFFER_SIZE = 512
IDENTIFY_BUFFER_SIZE = 512
READ_THRESHOLD_BUFFER_SIZE = 512
SMART_LOG_SECTOR_SIZE = 512

READ_ATTRIBUTES = $D0
READ_THRESHOLDS = $D1
ENABLE_DISABLE_AUTOSAVE = $D2
SAVE_ATTRIBUTE_VALUES = $D3
EXECUTE_OFFLINE_DIAGS = $D4
SMART_READ_LOG = $D5
SMART_WRITE_LOG = $d6
ENABLE_SMART = $D8
DISABLE_SMART = $D9
RETURN_SMART_STATUS = $DA
ENABLE_DISABLE_AUTO_OFFLINE = $DB

IOCTL_DISK_RESET_SNAPSHOT_INFO = $7C210

IOCTL_CHANGER_BASE = FILE_DEVICE_CHANGER

IOCTL_CHANGER_GET_PARAMETERS = $304000
IOCTL_CHANGER_GET_STATUS = $304004
IOCTL_CHANGER_GET_PRODUCT_DATA = $304008
IOCTL_CHANGER_SET_ACCESS = $30C010
IOCTL_CHANGER_GET_ELEMENT_STATUS = $30C014
IOCTL_CHANGER_INITIALIZE_ELEMENT_STATUS = $304018
IOCTL_CHANGER_SET_POSITION = $30401C
IOCTL_CHANGER_EXCHANGE_MEDIUM = $304020
IOCTL_CHANGER_MOVE_MEDIUM = $304024
IOCTL_CHANGER_REINITIALIZE_TRANSPORT = $304028
IOCTL_CHANGER_QUERY_VOLUME_TAGS = $30C02C

MAX_VOLUME_ID_SIZE = 36
MAX_VOLUME_TEMPLATE_SIZE = 40

VENDOR_ID_LENGTH = 8
PRODUCT_ID_LENGTH = 16
REVISION_LENGTH = 4
SERIAL_NUMBER_LENGTH = 32

AllElements = 0
ChangerTransport = 1
ChangerSlot = 2
ChangerIEPort = 3
ChangerDrive = 4
ChangerDoor = 5
ChangerKeypad = 6
ChangerMaxElement = 7

struct CHANGER_ELEMENT
       ElementType dd ?
       ElementAddress dd ?
ends

struct CHANGER_ELEMENT_LIST
       Element CHANGER_ELEMENT
       NumberOfElements dd ?
ends

CHANGER_BAR_CODE_SCANNER_INSTALLED = 1
CHANGER_INIT_ELEM_STAT_WITH_RANGE = 2
CHANGER_CLOSE_IEPORT = 4
CHANGER_OPEN_IEPORT = 8
CHANGER_STATUS_NON_VOLATILE = $10
CHANGER_EXCHANGE_MEDIA = $20
CHANGER_CLEANER_SLOT = $40
CHANGER_LOCK_UNLOCK = $80
CHANGER_CARTRIDGE_MAGAZINE = $100
CHANGER_MEDIUM_FLIP = $200
CHANGER_POSITION_TO_ELEMENT = $400
CHANGER_REPORT_IEPORT_STATE = $800
CHANGER_STORAGE_DRIVE = $1000
CHANGER_STORAGE_IEPORT = $2000
CHANGER_STORAGE_SLOT = $4000
CHANGER_STORAGE_TRANSPORT = $8000
CHANGER_DRIVE_CLEANING_REQUIRED = $10000
CHANGER_PREDISMOUNT_EJECT_REQUIRED = $20000
CHANGER_CLEANER_ACCESS_NOT_VALID = $40000
CHANGER_PREMOUNT_EJECT_REQUIRED = $80000
CHANGER_VOLUME_IDENTIFICATION = $100000
CHANGER_VOLUME_SEARCH = $200000
CHANGER_VOLUME_ASSERT = $400000
CHANGER_VOLUME_REPLACE = $800000
CHANGER_VOLUME_UNDEFINE = $1000000
CHANGER_SERIAL_NUMBER_VALID = $4000000
CHANGER_DEVICE_REINITIALIZE_CAPABLE = $8000000
CHANGER_KEYPAD_ENABLE_DISABLE = $10000000
CHANGER_DRIVE_EMPTY_ON_DOOR_ACCESS  = $20000000
CHANGER_RESERVED_BIT = $80000000

CHANGER_PREDISMOUNT_ALIGN_TO_SLOT = $80000001
CHANGER_PREDISMOUNT_ALIGN_TO_DRIVE = $80000002
CHANGER_CLEANER_AUTODISMOUNT = $80000004
CHANGER_TRUE_EXCHANGE_CAPABLE = $80000008
CHANGER_SLOTS_USE_TRAYS = $80000010
CHANGER_RTN_MEDIA_TO_ORIGINAL_ADDR = $80000020
CHANGER_CLEANER_OPS_NOT_SUPPORTED = $80000040
CHANGER_IEPORT_USER_CONTROL_OPEN = $80000080
CHANGER_IEPORT_USER_CONTROL_CLOSE = $80000100
CHANGER_MOVE_EXTENDS_IEPORT = $80000200
CHANGER_MOVE_RETRACTS_IEPORT = $80000400

CHANGER_TO_TRANSPORT = 1
CHANGER_TO_SLOT = 2
CHANGER_TO_IEPORT = 4
CHANGER_TO_DRIVE = 8

LOCK_UNLOCK_IEPORT = 1
LOCK_UNLOCK_DOOR = 2
LOCK_UNLOCK_KEYPAD = 4

struct GET_CHANGER_PARAMETERS
       Size dd ?
       NumberTransportElements dw ?
       NumberStorageElements dw ?
       NumberCleanerSlots dw ?
       NumberIEElements dw ?
       NumberDataTransferElements dw ?
       NumberOfDoors dw ?
       FirstSlotNumber dw ?
       FirstDriveNumber dw ?
       FirstTransportNumber dw ?
       FirstIEPortNumber dw ?
       FirstCleanerSlotAddress dw ?
       MagazineSize dw ?
       DriveCleanTimeout dd ?
       Features0 dd ?
       Features1 dd ?
       MoveFromTransport db ?
       MoveFromSlot db ?
       MoveFromIePort db ?
       MoveFromDrive db ?
       ExchangeFromTransport db ?
       ExchangeFromSlot db ?
       ExchangeFromIePort db ?
       ExchangeFromDrive db ?
       LockUnlockCapabilities db ?
       PositionCapabilities db ?
       Reserved1 db ?,?
       Reserved2 dd ?,?
ends

struct CHANGER_PRODUCT_DATA
       VendorId db VENDOR_ID_LENGTH dup(?)
       ProductId db PRODUCT_ID_LENGTH dup(?)
       Revision db REVISION_LENGTH dup(?)
       SerialNumber db SERIAL_NUMBER_LENGTH dup(?)
       DeviceType db ?
ends

LOCK_ELEMENT = 0
UNLOCK_ELEMENT = 1
EXTEND_IEPORT = 2
RETRACT_IEPORT = 3

struct CHANGER_SET_ACCESS
       Element CHANGER_ELEMENT
       Control dd ?
ends

struct CHANGER_READ_ELEMENT_STATUS
       ElementList CHANGER_ELEMENT_LIST
       VolumeTagInfo db ?,?,?,?
ends

struct CHANGER_ELEMENT_STATUS
       Element CHANGER_ELEMENT
       SrcElementAddress CHANGER_ELEMENT
       Flags dd ?
       ExceptionCode dd ?
       TargetId db ?
       Lun db ?
       Reserved dw ?
       PrimaryVolumeID db MAX_VOLUME_ID_SIZE dup(?)
       AlternateVolumeID db MAX_VOLUME_ID_SIZE dup(?)
ends

struct CHANGER_ELEMENT_STATUS_EX
       Element CHANGER_ELEMENT
       SrcElementAddress CHANGER_ELEMENT
       Flags dd ?
       ExceptionCode dd ?
       TargetId db ?
       Lun db ?
       Reserved dw ?
       PrimaryVolumeID db MAX_VOLUME_ID_SIZE dup(?)
       AlternateVolumeID db MAX_VOLUME_ID_SIZE dup(?)
       VendorIdentification db VENDOR_ID_LENGTH dup(?)
       ProductIdentification db PRODUCT_ID_LENGTH dup(?)
       SerialNumber db SERIAL_NUMBER_LENGTH dup(?)
ends

ELEMENT_STATUS_FULL = 1
ELEMENT_STATUS_IMPEXP = 2
ELEMENT_STATUS_EXCEPT = 4
ELEMENT_STATUS_ACCESS = 8
ELEMENT_STATUS_EXENAB = $10
ELEMENT_STATUS_INENAB = $20
ELEMENT_STATUS_PRODUCT_DATA = $40
ELEMENT_STATUS_LUN_VALID = $1000
ELEMENT_STATUS_ID_VALID = $2000
ELEMENT_STATUS_NOT_BUS = $8000
ELEMENT_STATUS_INVERT = $400000
ELEMENT_STATUS_SVALID = $800000
ELEMENT_STATUS_PVOLTAG = $10000000
ELEMENT_STATUS_AVOLTAG = $20000000

ERROR_LABEL_UNREADABLE = 1
ERROR_LABEL_QUESTIONABLE = 2
ERROR_SLOT_NOT_PRESENT = 4
ERROR_DRIVE_NOT_INSTALLED = 8
ERROR_TRAY_MALFUNCTION = $10
ERROR_INIT_STATUS_NEEDED = $11
ERROR_UNHANDLED_ERROR = $FFFFFFFF

struct CHANGER_INITIALIZE_ELEMENT_STATUS
       ElementList CHANGER_ELEMENT_LIST
       BarCodeScan db ?,?,?,?
ends

struct CHANGER_SET_POSITION
       Transport CHANGER_ELEMENT
       Destination CHANGER_ELEMENT
       Flip db ?,?,?,?
ends

struct CHANGER_EXCHANGE_MEDIUM
       Transport CHANGER_ELEMENT
       Source CHANGER_ELEMENT
       Destination1 CHANGER_ELEMENT
       Destination2 CHANGER_ELEMENT
       Flip1 db ?
       Flip2 db ?,?,?
ends

struct CHANGER_MOVE_MEDIUM
       Transport CHANGER_ELEMENT
       Source CHANGER_ELEMENT
       Destination CHANGER_ELEMENT
       Flip db ?,?,?,?
ends

struct CHANGER_SEND_VOLUME_TAG_INFORMATION
       StartingElement CHANGER_ELEMENT
       ActionCode dd ?
       VolumeIDTemplate db MAX_VOLUME_TEMPLATE_SIZE dup(?)
ends

struct READ_ELEMENT_ADDRESS_INFO
       NumberOfElements dd ?
       ElementStatus CHANGER_ELEMENT_STATUS
ends

SEARCH_ALL = 0
SEARCH_PRIMARY = 1
SEARCH_ALTERNATE = 2
SEARCH_ALL_NO_SEQ = 4
SEARCH_PRI_NO_SEQ = 5
SEARCH_ALT_NO_SEQ = 6

ASSERT_PRIMARY = 8
ASSERT_ALTERNATE = 9

REPLACE_PRIMARY = $A
REPLACE_ALTERNATE = $B

UNDEFINE_PRIMARY = $C
UNDEFINE_ALTERNATE = $D

DeviceProblemNone = 0
DeviceProblemHardware = 1
DeviceProblemCHMError = 2
DeviceProblemDoorOpen = 3
DeviceProblemCalibrationError = 4
DeviceProblemTargetFailure = 5
DeviceProblemCHMMoveError = 6
DeviceProblemCHMZeroError = 7
DeviceProblemCartridgeInsertError = 8
DeviceProblemPositionError = 9
DeviceProblemSensorError = 10
DeviceProblemCartridgeEjectError = 11
DeviceProblemGripperError = 12
DeviceProblemDriveError = 13

IOCTL_SERIAL_LSRMST_INSERT = $1B007C

IOCTL_SERENUM_EXPOSE_HARDWARE = $370200
IOCTL_SERENUM_REMOVE_HARDWARE = $370204
IOCTL_SERENUM_PORT_DESC = $370208
IOCTL_SERENUM_GET_PORT_NAME = $37020C

SERIAL_LSRMST_ESCAPE = 0
SERIAL_LSRMST_LSR_DATA = 1
SERIAL_LSRMST_LSR_NODATA = 2
SERIAL_LSRMST_MST = 3

SERIAL_IOC_FCR_FIFO_ENABLE = 1
SERIAL_IOC_FCR_RCVR_RESET = 2
SERIAL_IOC_FCR_XMIT_RESET = 4
SERIAL_IOC_FCR_DMA_MODE = 8
SERIAL_IOC_FCR_RES1 = $10
SERIAL_IOC_FCR_RES2 = $20
SERIAL_IOC_FCR_RCVR_TRIGGER_LSB = $40
SERIAL_IOC_FCR_RCVR_TRIGGER_MSB = $80

SERIAL_IOC_MCR_DTR = 1
SERIAL_IOC_MCR_RTS = 2
SERIAL_IOC_MCR_OUT1 = 4
SERIAL_IOC_MCR_OUT2 = 8
SERIAL_IOC_MCR_LOOP = $10

FSCTL_REQUEST_OPLOCK_LEVEL_1 = $90000
FSCTL_REQUEST_OPLOCK_LEVEL_2 = $90004
FSCTL_REQUEST_BATCH_OPLOCK = $90008
FSCTL_OPLOCK_BREAK_ACKNOWLEDGE = $9000C
FSCTL_OPBATCH_ACK_CLOSE_PENDING = $90010
FSCTL_OPLOCK_BREAK_NOTIFY = $90014
FSCTL_LOCK_VOLUME = $90018
FSCTL_UNLOCK_VOLUME = $9001C
FSCTL_DISMOUNT_VOLUME = $90020
FSCTL_IS_VOLUME_MOUNTED = $90028
FSCTL_IS_PATHNAME_VALID = $9002C
FSCTL_MARK_VOLUME_DIRTY = $90030
FSCTL_QUERY_RETRIEVAL_POINTERS	= $9003B
FSCTL_GET_COMPRESSION = $9003C
FSCTL_SET_COMPRESSION = $9C040
FSCTL_MARK_AS_SYSTEM_HIVE = $9004F
FSCTL_OPLOCK_BREAK_ACK_NO_2 = $90050
FSCTL_INVALIDATE_VOLUMES = $90054
FSCTL_QUERY_FAT_BPB = $90058
FSCTL_REQUEST_FILTER_OPLOCK = $9005C
FSCTL_FILESYSTEM_GET_STATISTICS = $90060

FSCTL_GET_NTFS_VOLUME_DATA = $90064
FSCTL_GET_NTFS_FILE_RECORD = $90068
FSCTL_GET_VOLUME_BITMAP = $9006F
FSCTL_GET_RETRIEVAL_POINTERS = $90073
FSCTL_MOVE_FILE = $90074
FSCTL_IS_VOLUME_DIRTY = $90078
FSCTL_ALLOW_EXTENDED_DASD_IO = $90083

FSCTL_FIND_FILES_BY_SID = $9008F
FSCTL_SET_OBJECT_ID = $90098
FSCTL_GET_OBJECT_ID = $9009C
FSCTL_DELETE_OBJECT_ID = $900A0
FSCTL_SET_REPARSE_POINT = $900A4
FSCTL_GET_REPARSE_POINT = $900A8
FSCTL_DELETE_REPARSE_POINT = $900AC
FSCTL_ENUM_USN_DATA = $900B3
FSCTL_SECURITY_ID_CHECK = $940B7
FSCTL_READ_USN_JOURNAL = $900BB
FSCTL_SET_OBJECT_ID_EXTENDED = $900BC
FSCTL_CREATE_OR_GET_OBJECT_ID = $900C0
FSCTL_SET_SPARSE = $900C4
FSCTL_SET_ZERO_DATA = $980C8
FSCTL_QUERY_ALLOCATED_RANGES = $940CF
FSCTL_ENABLE_UPGRADE = $980D0
FSCTL_SET_ENCRYPTION = $900D7
FSCTL_ENCRYPTION_FSCTL_IO = $900DB
FSCTL_WRITE_RAW_ENCRYPTED = $900DF
FSCTL_READ_RAW_ENCRYPTED = $900E3
FSCTL_CREATE_USN_JOURNAL = $900E7
FSCTL_READ_FILE_USN_DATA = $900EB
FSCTL_WRITE_USN_CLOSE_RECORD = $900EF
FSCTL_EXTEND_VOLUME = $900F0
FSCTL_QUERY_USN_JOURNAL = $900F4
FSCTL_DELETE_USN_JOURNAL = $900F8
FSCTL_MARK_HANDLE = $900FC
FSCTL_SIS_COPYFILE = $90100
FSCTL_SIS_LINK_FILES = $9C104
FSCTL_RECALL_FILE = $90117
FSCTL_READ_FROM_PLEX = $9411E
FSCTL_FILE_PREFETCH = $90120

FSCTL_MAKE_MEDIA_COMPATIBLE = $98130
FSCTL_SET_DEFECT_MANAGEMENT = $98134
FSCTL_QUERY_SPARING_INFO = $90138
FSCTL_QUERY_ON_DISK_VOLUME_INFO = $9013C
FSCTL_SET_VOLUME_COMPRESSION_STATE = $90140
FSCTL_TXFS_MODIFY_RM = $98144
FSCTL_TXFS_QUERY_RM_INFORMATION = $94148
FSCTL_TXFS_ROLLFORWARD_REDO = $98150
FSCTL_TXFS_ROLLFORWARD_UNDO = $98154
FSCTL_TXFS_START_RM = $98158
FSCTL_TXFS_SHUTDOWN_RM = $9815C
FSCTL_TXFS_READ_BACKUP_INFORMATION = $94160
FSCTL_TXFS_WRITE_BACKUP_INFORMATION = $98164
FSCTL_TXFS_CREATE_SECONDARY_RM = $98168
FSCTL_TXFS_GET_METADATA_INFO = $9416C
FSCTL_TXFS_GET_TRANSACTED_VERSION = $94170
FSCTL_TXFS_SAVEPOINT_INFORMATION = $98178
FSCTL_TXFS_CREATE_MINIVERSION = $9817C
FSCTL_TXFS_TRANSACTION_ACTIVE = $9418C
FSCTL_SET_ZERO_ON_DEALLOCATION = $90194
FSCTL_SET_REPAIR = $90198
FSCTL_GET_REPAIR = $9019C
FSCTL_WAIT_FOR_REPAIR = $901A0
FSCTL_INITIATE_REPAIR = $901A8
FSCTL_CSC_INTERNAL = $901AF
FSCTL_SHRINK_VOLUME = $901B0
FSCTL_SET_SHORT_NAME_BEHAVIOR = $901B4
FSCTL_DFSR_SET_GHOST_HANDLE_STATE = $901B8

FSCTL_TXFS_LIST_TRANSACTION_LOCKED_FILES = $941E0
FSCTL_TXFS_LIST_TRANSACTIONS = $941E4
FSCTL_QUERY_PAGEFILE_ENCRYPTION = $901E8
FSCTL_RESET_VOLUME_ALLOCATION_HINTS = $901EC
FSCTL_QUERY_DEPENDENT_VOLUME = $901F0
FSCTL_SD_GLOBAL_CHANGE = $901F4
FSCTL_TXFS_READ_BACKUP_INFORMATION2 = $901F8
FSCTL_LOOKUP_STREAM_FROM_CLUSTER = $901FC
FSCTL_TXFS_WRITE_BACKUP_INFORMATION2 = $90200
FSCTL_FILE_TYPE_NOTIFICATION = $90204
FSCTL_GET_BOOT_AREA_INFO = $90230
FSCTL_GET_RETRIEVAL_POINTER_BASE = $90234
FSCTL_SET_PERSISTENT_VOLUME_STATE = $90238
FSCTL_QUERY_PERSISTENT_VOLUME_STATE = $9023C
FSCTL_REQUEST_OPLOCK = $90240
FSCTL_CSV_TUNNEL_REQUEST = $90244
FSCTL_IS_CSV_FILE = $90248
FSCTL_QUERY_FILE_SYSTEM_RECOGNITION = $9024C
FSCTL_CSV_GET_VOLUME_PATH_NAME = $90250
FSCTL_CSV_GET_VOLUME_NAME_FOR_VOLUME_MOUNT_POINT = $90254
FSCTL_CSV_GET_VOLUME_PATH_NAMES_FOR_VOLUME_NAME = $90258
FSCTL_IS_FILE_ON_CSV_VOLUME = $9025C
;FSCTL_MARK_AS_SYSTEM_HIVE = FSCTL_SET_BOOTLOADER_ACCESSED

struct CSV_NAMESPACE_INFO
       Version dd ?
       DeviceNumber dd ?
       StartingOffset dq ?
       SectorSize dd ?,?
ends

CSV_NAMESPACE_INFO_V1 = sizeof.CSV_NAMESPACE_INFO
CSV_INVALID_DEVICE_NUMBER = $FFFFFFFF

struct PATHNAME_BUFFER
       PathNameLength dd ?
       Name dw ?,?
ends

struct FSCTL_QUERY_FAT_BPB_BUFFER
       First0x24BytesOfBootSector db $24 dup(?)
ends

struct NTFS_VOLUME_DATA_BUFFER
       VolumeSerialNumber dq ?
       NumberSectors dq ?
       TotalClusters dq ?
       FreeClusters dq ?
       TotalReserved dq ?
       BytesPerSector dd ?
       BytesPerCluster dd ?
       BytesPerFileRecordSegment dd ?
       ClustersPerFileRecordSegment dd ?
       MftValidDataLength dq ?
       MftStartLcn dq ?
       Mft2StartLcn dq ?
       MftZoneStart dq ?
       MftZoneEnd dq ?
ends

struct NTFS_EXTENDED_VOLUME_DATA
       ByteCount dd ?
       MajorVersion dw ?
       MinorVersion dw ?
ends

struct STARTING_LCN_INPUT_BUFFER
       StartingLcn dq ?
ends

struct VOLUME_BITMAP_BUFFER
       StartingLcn dq ?
       BitmapSize dq ?
       Buffer db 8 dup(?)
ends

struct STARTING_VCN_INPUT_BUFFER
       StartingVcn dq ?
ends

struct RETRIEVAL_POINTERS_BUFFER
       ExtentCount dd ?,?
       StartingVcn dq ?
       struct
	      NextVcn dq ?
	      Lcn dq ?
       ends
ends

struct NTFS_FILE_RECORD_INPUT_BUFFER
       FileReferenceNumber dq ?
ends

struct NTFS_FILE_RECORD_OUTPUT_BUFFER
       FileReferenceNumber dq ?
       FileRecordLength dd ?
       FileRecordBuffer db ?,?,?,?
ends

struct MOVE_FILE_DATA
       FileHandle dd ?,?
       StartingVcn dq ?
       StartingLcn dq ?
       ClusterCount dd ?,?
ends

struct MOVE_FILE_RECORD_DATA
       FileHandle dd ?,?
       SourceFileRecord dq ?
       TargetFileRecord dq ?
ends

struct MOVE_FILE_DATA32
       FileHandle dd ?,?
       StartingVcn dq ?
       StartingLcn dq ?
       ClusterCount dd ?,?
ends

struct FIND_BY_SID_DATA
       Restart dd ?
       Sid SID
ends

struct FIND_BY_SID_OUTPUT
       NextEntryOffset dd ?
       FileIndex dd ?
       FileNameLength dd ?
       FileName dw ?,?
ends

struct MFT_ENUM_DATA
       StartFileReferenceNumber dq ?
       LowUsn dq ?
       HighUsn dq ?
ends

struct CREATE_USN_JOURNAL_DATA
       MaximumSize dq ?
       AllocationDelta dq ?
ends

struct READ_USN_JOURNAL_DATA
       StartUsn dq ?
       ReasonMask dd ?
       ReturnOnlyOnClose dd ?
       Timeout dq ?
       BytesToWaitFor dq ?
       UsnJournalID dq ?
ends

struct USN_RECORD
       RecordLength dd ?
       MajorVersion dw ?
       MinorVersion dw ?
       FileReferenceNumber dq ?
       ParentFileReferenceNumber dq ?
       Usn dq ?
       TimeStamp dq ?
       Reason dd ?
       SourceInfo dd ?
       SecurityId dd ?
       FileAttributes dd ?
       FileNameLength dw ?
       FileNameOffset dw ?
       FileName dw ?,?
ends

USN_PAGE_SIZE = $1000

USN_REASON_DATA_OVERWRITE = 1
USN_REASON_DATA_EXTEND = 2
USN_REASON_DATA_TRUNCATION = 4
USN_REASON_NAMED_DATA_OVERWRITE = $10
USN_REASON_NAMED_DATA_EXTEND = $20
USN_REASON_NAMED_DATA_TRUNCATION = $40
USN_REASON_FILE_CREATE = $100
USN_REASON_FILE_DELETE = $200
USN_REASON_EA_CHANGE = $400
USN_REASON_SECURITY_CHANGE = $800
USN_REASON_RENAME_OLD_NAME = $1000
USN_REASON_RENAME_NEW_NAME = $2000
USN_REASON_INDEXABLE_CHANGE = $4000
USN_REASON_BASIC_INFO_CHANGE = $8000
USN_REASON_HARD_LINK_CHANGE = $10000
USN_REASON_COMPRESSION_CHANGE = $20000
USN_REASON_ENCRYPTION_CHANGE = $40000
USN_REASON_OBJECT_ID_CHANGE = $80000
USN_REASON_REPARSE_POINT_CHANGE = $100000
USN_REASON_STREAM_CHANGE = $200000
USN_REASON_TRANSACTED_CHANGE = $400000
USN_REASON_CLOSE = $80000000

struct USN_JOURNAL_DATA
       UsnJournalID dq ?
       FirstUsn dq ?
       NextUsn dq ?
       LowestValidUsn dq ?
       MaxUsn dq ?
       MaximumSize dq ?
       AllocationDelta dq ?
ends

struct DELETE_USN_JOURNAL_DATA
       UsnJournalID dq ?
       DeleteFlags dd ?,?
ends

USN_DELETE_FLAG_DELETE = 1
USN_DELETE_FLAG_NOTIFY = 2
USN_DELETE_VALID_FLAGS = 3

struct MARK_HANDLE_INFO
       UsnSourceInfo dd ?
       VolumeHandle dd ?
       HandleInfo dd ?
ends

struct MARK_HANDLE_INFO32 MARK_HANDLE_INFO
ends

USN_SOURCE_DATA_MANAGEMENT = 1
USN_SOURCE_AUXILIARY_DATA = 2
USN_SOURCE_REPLICATION_MANAGEMENT = 4

MARK_HANDLE_PROTECT_CLUSTERS = 1
MARK_HANDLE_TXF_SYSTEM_LOG = 4
MARK_HANDLE_NOT_TXF_SYSTEM_LOG = 8
MARK_HANDLE_REALTIME = $20
MARK_HANDLE_NOT_REALTIME = $40

NO_8DOT3_NAME_PRESENT = 1
REMOVED_8DOT3_NAME = 2

PERSISTENT_VOLUME_STATE_SHORT_NAME_CREATION_DISABLED = 1

struct BULK_SECURITY_TEST_DATA
       DesiredAccess dd ?
       SecurityIds dd ?
ends

VOLUME_IS_DIRTY = 1
VOLUME_UPGRADE_SCHEDULED = 2
VOLUME_SESSION_OPEN = 4

struct FILE_PREFETCH
       Type dd ?
       Count dd ?
       Prefetch dq ?
ends

struct FILE_PREFETCH_EX
       Type dd ?
       Count dd ?
       Context dd ?,?
       Prefetch dq ?
ends

FILE_PREFETCH_TYPE_FOR_CREATE = 1
FILE_PREFETCH_TYPE_FOR_DIRENUM = 2
FILE_PREFETCH_TYPE_FOR_CREATE_EX = 3
FILE_PREFETCH_TYPE_FOR_DIRENUM_EX = 4
FILE_PREFETCH_TYPE_MAX = 4

struct FILESYSTEM_STATISTICS
       FileSystemType dw ?
       Version dw ?
       SizeOfCompleteStructure dd ?
       UserFileReads dd ?
       UserFileReadBytes dd ?
       UserDiskReads dd ?
       UserFileWrites dd ?
       UserFileWriteBytes dd ?
       UserDiskWrites dd ?
       MetaDataReads dd ?
       MetaDataReadBytes dd ?
       MetaDataDiskReads dd ?
       MetaDataWrites dd ?
       MetaDataWriteBytes dd ?
       MetaDataDiskWrites dd ?
ends

FILESYSTEM_STATISTICS_TYPE_NTFS = 1
FILESYSTEM_STATISTICS_TYPE_FAT = 2
FILESYSTEM_STATISTICS_TYPE_EXFAT = 3

struct FAT_STATISTICS
       CreateHits dd ?
       SuccessfulCreates dd ?
       FailedCreates dd ?
       NonCachedReads dd ?
       NonCachedReadBytes dd ?
       NonCachedWrites dd ?
       NonCachedWriteBytes dd ?
       NonCachedDiskReads dd ?
       NonCachedDiskWrites dd ?
ends

struct EXFAT_STATISTICS
       CreateHits dd ?
       SuccessfulCreates dd ?
       FailedCreates dd ?
       NonCachedReads dd ?
       NonCachedReadBytes dd ?
       NonCachedWrites dd ?
       NonCachedWriteBytes dd ?
       NonCachedDiskReads dd ?
       NonCachedDiskWrites dd ?
ends

struct NTFS_STATISTICS
       LogFileFullExceptions dd ?
       OtherExceptions dd ?
       MftReads dd ?
       MftReadBytes dd ?
       MftWrites dd ?
       MftWriteBytes dd ?
       struct
	   mwul_Write dw ?
	   mwul_Create dw ?
	   mwul_SetInfo dw ?
	   mwul_Flush dw ?
       ends
       MftWritesFlushForLogFileFull dw ?
       MftWritesLazyWriter dw ?
       MftWritesUserRequest dw ?,?
       Mft2Writes dd ?
       Mft2WriteBytes dd ?
       struct
	   m2wul_Write dw ?
	   m2wul_Create dw ?
	   m2wul_SetInfo dw ?
	   m2wul_Flush dw ?
       ends
       Mft2WritesFlushForLogFileFull dw ?
       Mft2WritesLazyWriter dw ?
       Mft2WritesUserRequest dw ?,?
       RootIndexReads dd ?
       RootIndexReadBytes dd ?
       RootIndexWrites dd ?
       RootIndexWriteBytes dd ?
       BitmapReads dd ?
       BitmapReadBytes dd ?
       BitmapWrites dd ?
       BitmapWriteBytes dd ?
       BitmapWritesFlushForLogFileFull dw ?
       BitmapWritesLazyWriter dw ?
       BitmapWritesUserRequest dw ?
       struct
	   bwul_Write dw ?
	   bwul_Create dw ?
	   bwul_SetInfo dw ?
       ends
       MftBitmapReads dd ?
       MftBitmapReadBytes dd ?
       MftBitmapWrites dd ?
       MftBitmapWriteBytes dd ?
       MftBitmapWritesFlushForLogFileFull dw ?
       MftBitmapWritesLazyWriter dw ?
       MftBitmapWritesUserRequest dw ?
       struct
	   mbwul_Write dw ?
	   mbwul_Create dw ?
	   mbwul_SetInfo dw ?
	   mbwul_Flush dw ?,?
       ends
       UserIndexReads dd ?
       UserIndexReadBytes dd ?
       UserIndexWrites dd ?
       UserIndexWriteBytes dd ?
       LogFileReads dd ?
       LogFileReadBytes dd ?
       LogFileWrites dd ?
       LogFileWriteBytes dd ?
       struct
	   Calls dd ?
	   Clusters dd ?
	   Hints dd ?
	   RunsReturned dd ?
	   HintsHonored dd ?
	   HintsClusters dd ?
	   Cache dd ?
	   CacheClusters dd ?
	   CacheMiss dd ?
	   CacheMissClusters dd ?
       ends
ends

struct FILE_OBJECTID_BUFFER
       ObjectId db 16 dup(?)
       union
	   struct
	       BirthVolumeId db 16 dup(?)
	       BirthObjectId db 16 dup(?)
	       DomainId db 16 dup(?)
	   ends
	   ExtendedInfo db 48 dup(?)
       ends
ends

struct FILE_SET_SPARSE_BUFFER
       SetSparse db ?
ends

struct FILE_ZERO_DATA_INFORMATION
       FileOffset dq ?
       BeyondFinalZero dq ?
ends

struct FILE_ALLOCATED_RANGE_BUFFER
       FileOffset dq ?
       Length dq ?
ends

struct ENCRYPTION_BUFFER
       EncryptionOperation dd ?
       Private db ?,?,?,?
ends

FILE_SET_ENCRYPTION = 1
FILE_CLEAR_ENCRYPTION = 2
STREAM_SET_ENCRYPTION = 3
STREAM_CLEAR_ENCRYPTION = 4
MAXIMUM_ENCRYPTION_VALUE = 4

struct DECRYPTIONSTATUSBUFFER
       NoEncryptedStreams db ?
ends

ENCRYPTION_FORMAT_DEFAULT = 1
COMPRESSION_FORMAT_SPARSE = $4000

struct REQUEST_RAW_ENCRYPTED_DATA
       FileOffset dq ?
       Length dd ?,?
ends

struct ENCRYPTED_DATA_INFO
       StartingFileOffset dq ?
       OutputBufferOffset dd ?
       BytesWithinFileSize dd ?
       BytesWithinValidDataLength dd ?
       CompressionFormat dw ?
       DataUnitShift db ?
       ChunkShift db ?
       ClusterShift db ?
       EncryptionFormat db ?
       NumberOfDataBlocks dw ?
       DataBlockSize dd ANYSIZE_ARRAY dup(?)
ends

struct PLEX_READ_DATA_REQUEST
       ByteOffset dq ?
       ByteLength dd ?
       PlexNumber dd ?
ends

struct SI_COPYFILE
       SourceFileNameLength dd ?
       DestinationFileNameLength dd ?
       Flags dd ?
       FileNameBuffer dw ?,?
ends

COPYFILE_SIS_LINK = 1
COPYFILE_SIS_REPLACE = 2
COPYFILE_SIS_FLAGS = 3

struct FILE_MAKE_COMPATIBLE_BUFFER
       CloseDisc db ?
ends

struct FILE_SET_DEFECT_MGMT_BUFFER
       Disable db ?
ends

struct FILE_QUERY_SPARING_BUFFER
       SparingUnitBytes dd ?
       SoftwareSparing db ?,?,?,?
       TotalSpareBlocks dd ?
       FreeSpareBlocks dd ?
ends

struct FILE_QUERY_ON_DISK_VOL_INFO_BUFFER
       DirectoryCount dq ?
       FileCount dq ?
       FsFormatMajVersion dw ?
       FsFormatMinVersion dw ?
       FsFormatName dw 14 dup(?)
       FormatTime dq ?
       LastUpdateTime dq ?
       CopyrightInfo dw 34 dup(?)
       AbstractInfo dw 34 dup(?)
       FormattingImplementationInfo dw 34 dup(?)
       LastModifyingImplementationInfo dw 34 dup(?)
ends

SET_REPAIR_ENABLED = 1
SET_REPAIR_VOLUME_BITMAP_SCAN = 2
SET_REPAIR_DELETE_CROSSLINK = 4
SET_REPAIR_WARN_ABOUT_DATA_LOSS = 8
SET_REPAIR_DISABLED_AND_BUGCHECK_ON_CORRUPT = $10
SET_REPAIR_VALID_MASK = $1F

ShrinkPrepare = 1
ShrinkCommit = 2
ShrinkAbort = 3

struct SHRINK_VOLUME_INFORMATION
       ShrinkRequestType dd ?,?
       Flags dq ?
       NewNumberOfSectors dq ?
ends

TXFS_RM_FLAG_LOGGING_MODE = 1
TXFS_RM_FLAG_RENAME_RM = 2
TXFS_RM_FLAG_LOG_CONTAINER_COUNT_MAX = 4
TXFS_RM_FLAG_LOG_CONTAINER_COUNT_MIN = 8
TXFS_RM_FLAG_LOG_GROWTH_INCREMENT_NUM_CONTAINERS = $10
TXFS_RM_FLAG_LOG_GROWTH_INCREMENT_PERCENT = $20
TXFS_RM_FLAG_LOG_AUTO_SHRINK_PERCENTAGE = $40
TXFS_RM_FLAG_LOG_NO_CONTAINER_COUNT_MAX = $80
TXFS_RM_FLAG_LOG_NO_CONTAINER_COUNT_MIN = $100
TXFS_RM_FLAG_GROW_LOG = $400
TXFS_RM_FLAG_SHRINK_LOG = $800
TXFS_RM_FLAG_ENFORCE_MINIMUM_SIZE = $1000
TXFS_RM_FLAG_PRESERVE_CHANGES = $2000
TXFS_RM_FLAG_RESET_RM_AT_NEXT_START = $4000
TXFS_RM_FLAG_DO_NOT_RESET_RM_AT_NEXT_START = $8000
TXFS_RM_FLAG_PREFER_CONSISTENCY = $10000
TXFS_RM_FLAG_PREFER_AVAILABILITY = $20000

TXFS_LOGGING_MODE_SIMPLE = 1
TXFS_LOGGING_MODE_FULL = 2

TXFS_TRANSACTION_STATE_NONE = 0
TXFS_TRANSACTION_STATE_ACTIVE = 1
TXFS_TRANSACTION_STATE_PREPARED = 2
TXFS_TRANSACTION_STATE_NOTACTIVE = 3

TXFS_MODIFY_RM_VALID_FLAGS = (TXFS_RM_FLAG_LOGGING_MODE or TXFS_RM_FLAG_RENAME_RM or TXFS_RM_FLAG_LOG_CONTAINER_COUNT_MAX or\
			      TXFS_RM_FLAG_LOG_CONTAINER_COUNT_MIN  or	TXFS_RM_FLAG_LOG_GROWTH_INCREMENT_NUM_CONTAINERS or\
			      TXFS_RM_FLAG_LOG_GROWTH_INCREMENT_PERCENT or TXFS_RM_FLAG_LOG_AUTO_SHRINK_PERCENTAGE or\
			      TXFS_RM_FLAG_LOG_NO_CONTAINER_COUNT_MAX or TXFS_RM_FLAG_LOG_NO_CONTAINER_COUNT_MIN or\
			      TXFS_RM_FLAG_SHRINK_LOG  or  TXFS_RM_FLAG_GROW_LOG or TXFS_RM_FLAG_ENFORCE_MINIMUM_SIZE or\
			      TXFS_RM_FLAG_PRESERVE_CHANGES or TXFS_RM_FLAG_RESET_RM_AT_NEXT_START or\
			      TXFS_RM_FLAG_DO_NOT_RESET_RM_AT_NEXT_START or TXFS_RM_FLAG_PREFER_CONSISTENCY or\
			      TXFS_RM_FLAG_PREFER_AVAILABILITY)

struct TXFS_MODIFY_RM
       Flags dd ?
       LogContainerCountMax dd ?
       LogContainerCountMin dd ?
       LogContainerCount dd ?
       LogGrowthIncrement dd ?
       LogAutoShrinkPercentage dd ?
       Reserved dq ?
       LoggingMode dw ?,?,?,?
ends

TXFS_RM_STATE_NOT_STARTED = 0
TXFS_RM_STATE_STARTING = 1
TXFS_RM_STATE_ACTIVE = 2
TXFS_RM_STATE_SHUTTING_DOWN = 3

TXFS_QUERY_RM_INFORMATION_VALID_FLAGS = (TXFS_RM_FLAG_LOG_GROWTH_INCREMENT_NUM_CONTAINERS or\
					 TXFS_RM_FLAG_LOG_GROWTH_INCREMENT_PERCENT or TXFS_RM_FLAG_LOG_NO_CONTAINER_COUNT_MAX or\
					 TXFS_RM_FLAG_LOG_NO_CONTAINER_COUNT_MIN or TXFS_RM_FLAG_RESET_RM_AT_NEXT_START or\
					 TXFS_RM_FLAG_DO_NOT_RESET_RM_AT_NEXT_START or TXFS_RM_FLAG_PREFER_CONSISTENCY or\
					 TXFS_RM_FLAG_PREFER_AVAILABILITY)

struct TXFS_QUERY_RM_INFORMATION
       BytesRequired dd ?,?
       TailLsn dq ?
       CurrentLsn dq ?
       ArchiveTailLsn dq ?
       LogContainerSize dq ?
       HighestVirtualClock dq ?
       LogContainerCount dd ?
       LogContainerCountMax dd ?
       LogContainerCountMin dd ?
       LogGrowthIncrement dd ?
       LogAutoShrinkPercentage dd ?
       Flags dd ?
       LoggingMode dw ?
       Reserved dw ?
       RmState dd ?
       LogCapacity dq ?
       LogFree dq ?
       TopsSize dq ?
       TopsUsed dq ?
       TransactionCount dq ?
       OnePCCount dq ?
       TwoPCCount dq ?
       NumberLogFileFull dq ?
       OldestTransactionAge dq ?
       RMName GUID
       TmLogPathOffset dd ?,?
ends

TXFS_ROLLFORWARD_REDO_FLAG_USE_LAST_REDO_LSN = 1
TXFS_ROLLFORWARD_REDO_FLAG_USE_LAST_VIRTUAL_CLOCK = 2
TXFS_ROLLFORWARD_REDO_VALID_FLAGS = (TXFS_ROLLFORWARD_REDO_FLAG_USE_LAST_REDO_LSN or TXFS_ROLLFORWARD_REDO_FLAG_USE_LAST_VIRTUAL_CLOCK)

struct TXFS_ROLLFORWARD_REDO_INFORMATION
       LastVirtualClock dq ?
       LastRedoLsn dq ?
       HighestRecoveryLsn dq ?
       Flags dd ?,?
ends

TXFS_START_RM_FLAG_LOG_CONTAINER_COUNT_MAX = 1
TXFS_START_RM_FLAG_LOG_CONTAINER_COUNT_MIN = 2
TXFS_START_RM_FLAG_LOG_CONTAINER_SIZE = 4
TXFS_START_RM_FLAG_LOG_GROWTH_INCREMENT_NUM_CONTAINERS = 8
TXFS_START_RM_FLAG_LOG_GROWTH_INCREMENT_PERCENT = $10
TXFS_START_RM_FLAG_LOG_AUTO_SHRINK_PERCENTAGE = $20
TXFS_START_RM_FLAG_LOG_NO_CONTAINER_COUNT_MAX = $40
TXFS_START_RM_FLAG_LOG_NO_CONTAINER_COUNT_MIN = $80
TXFS_START_RM_FLAG_RECOVER_BEST_EFFORT = $200
TXFS_START_RM_FLAG_LOGGING_MODE = $400
TXFS_START_RM_FLAG_PRESERVE_CHANGES = $800
TXFS_START_RM_FLAG_PREFER_CONSISTENCY = $1000
TXFS_START_RM_FLAG_PREFER_AVAILABILITY = $2000

TXFS_START_RM_VALID_FLAGS = (TXFS_START_RM_FLAG_LOG_CONTAINER_COUNT_MAX or TXFS_START_RM_FLAG_LOG_CONTAINER_COUNT_MIN or\
			     TXFS_START_RM_FLAG_LOG_CONTAINER_SIZE or TXFS_START_RM_FLAG_LOG_GROWTH_INCREMENT_NUM_CONTAINERS or\
			     TXFS_START_RM_FLAG_LOG_GROWTH_INCREMENT_PERCENT or TXFS_START_RM_FLAG_LOG_AUTO_SHRINK_PERCENTAGE or\
			     TXFS_START_RM_FLAG_RECOVER_BEST_EFFORT or TXFS_START_RM_FLAG_LOG_NO_CONTAINER_COUNT_MAX or\
			     TXFS_START_RM_FLAG_LOGGING_MODE or TXFS_START_RM_FLAG_PRESERVE_CHANGES or\
			     TXFS_START_RM_FLAG_PREFER_CONSISTENCY or TXFS_START_RM_FLAG_PREFER_AVAILABILITY)

struct TXFS_START_RM_INFORMATION
       Flags dd ?,?
       LogContainerSize dq ?
       LogContainerCountMin dd ?
       LogContainerCountMax dd ?
       LogGrowthIncrement dd ?
       LogAutoShrinkPercentage dd ?
       TmLogPathOffset dd ?
       TmLogPathLength dw ?
       LoggingMode dw ?
       LogPathLength dw ?
       Reserved dw ?
       LogPath dw ?,?
ends

struct TXFS_GET_METADATA_INFO_OUT
       struct
	   LowPart dq ?
	   HighPart dq ?
       ends
       LockingTransaction GUID
       LastLsn dq ?
       TransactionState dd ?,?
ends

TXFS_LIST_TRANSACTION_LOCKED_FILES_ENTRY_FLAG_CREATED = 1
TXFS_LIST_TRANSACTION_LOCKED_FILES_ENTRY_FLAG_DELETED = 2

struct TXFS_LIST_TRANSACTION_LOCKED_FILES_ENTRY
       Offset dq ?
       NameFlags dd ?,?
       FileId dq ?
       Reserved1 dd ?
       Reserved2 dd ?
       Reserved3 dq ?
       FileName dw ?,?,?,?
ends

struct TXFS_LIST_TRANSACTION_LOCKED_FILES
       KtmTransaction GUID
       NumberOfFiles dq ?
       BufferSizeRequired dq ?
       Offset dq ?
ends

struct TXFS_LIST_TRANSACTIONS_ENTRY
       TransactionId GUID
       TransactionState dd ?
       Reserved1 dd ?
       Reserved2 dd ?,?
       Reserved3 dq ?
ends

struct TXFS_LIST_TRANSACTIONS
       NumberOfTransactions dq ?
       BufferSizeRequired dq ?
ends

struct TXFS_READ_BACKUP_INFORMATION_OUT
       union
	   BufferLength dd ?
	   Buffer db ?
       ends
ends

struct TXFS_WRITE_BACKUP_INFORMATION
       Buffer db ?
ends

TXFS_TRANSACTED_VERSION_NONTRANSACTED = $FFFFFFFE
TXFS_TRANSACTED_VERSION_UNCOMMITTED = $FFFFFFFF

struct TXFS_GET_TRANSACTED_VERSION
       ThisBaseVersion dd ?
       LatestVersion dd ?
       ThisMiniVersion dw ?
       FirstMiniVersion dw ?
       LatestMiniVersion dw ?,?
ends

TXFS_SAVEPOINT_SET = 1
TXFS_SAVEPOINT_ROLLBACK = 2
TXFS_SAVEPOINT_CLEAR = 4
TXFS_SAVEPOINT_CLEAR_ALL = $10

struct TXFS_SAVEPOINT_INFORMATION
       KtmTransaction dd ?
       ActionCode dd ?
       SavepointId dd ?
ends

struct TXFS_CREATE_MINIVERSION_INFO
       StructureVersion dw ?
       StructureLength dw ?
       BaseVersion dd ?
       MiniVersion dw ?,?
ends

struct TXFS_TRANSACTION_ACTIVE_INFO
       TransactionsActiveAtSnapshot db ?
ends

struct BOOT_AREA_INFO
       BootSectorCount dd ?,?
       struct
	   Offset dq ?,?
       ends
ends

struct RETRIEVAL_POINTER_BASE
       FileAreaOffset dq ?
ends

struct FILE_FS_PERSISTENT_VOLUME_INFORMATION
       VolumeFlags dd ?
       FlagMask dd ?
       Version dd ?
       Reserved dd ?
ends

struct FILE_SYSTEM_RECOGNITION_INFORMATION
       FileSystem db 9 dup(?)
ends

OPLOCK_LEVEL_CACHE_READ = 1
OPLOCK_LEVEL_CACHE_HANDLE = 2
OPLOCK_LEVEL_CACHE_WRITE = 4

REQUEST_OPLOCK_INPUT_FLAG_REQUEST = 1
REQUEST_OPLOCK_INPUT_FLAG_ACK = 2
REQUEST_OPLOCK_INPUT_FLAG_COMPLETE_ACK_ON_CLOSE = 4
REQUEST_OPLOCK_CURRENT_VERSION = 1

struct REQUEST_OPLOCK_INPUT_BUFFER
       StructureVersion dw ?
       StructureLength dw ?
       RequestedOplockLevel dd ?
       Flags dd ?
ends

REQUEST_OPLOCK_OUTPUT_FLAG_ACK_REQUIRED = 1
REQUEST_OPLOCK_OUTPUT_FLAG_MODES_PROVIDED = 2

struct REQUEST_OPLOCK_OUTPUT_BUFFER
       StructureVersion dw ?
       StructureLength dw ?
       OriginalOplockLevel dd ?
       NewOplockLevel dd ?
       Flags dd ?
       AccessMode dd ?
       ShareMode dw ?,?
ends

SD_GLOBAL_CHANGE_TYPE_MACHINE_SID = 1

struct SD_CHANGE_MACHINE_SID_INPUT
       CurrentMachineSIDOffset dw ?
       CurrentMachineSIDLength dw ?
       NewMachineSIDOffset dw ?
       NewMachineSIDLength dw ?
ends

struct SD_CHANGE_MACHINE_SID_OUTPUT
       NumSDChangedSuccess dq ?
       NumSDChangedFail dq ?
       NumSDUnused dq ?
       NumSDTotal dq ?
       NumMftSDChangedSuccess dq ?
       NumMftSDChangedFail dq ?
       NumMftSDTotal dq ?
ends

struct SD_GLOBAL_CHANGE_INPUT
       Flags dd ?
       ChangeType dd ?
       union
	   SdChange SD_CHANGE_MACHINE_SID_INPUT
       ends
ends

struct SD_GLOBAL_CHANGE_OUTPUT
       Flags dd ?
       ChangeType dd ?
       union
	   SdChange SD_CHANGE_MACHINE_SID_OUTPUT
       ends
ends

ENCRYPTED_DATA_INFO_SPARSE_FILE = 1

struct EXTENDED_ENCRYPTED_DATA_INFO
       ExtendedCode dd ?
       Length dd ?
       Flags dd ?
       Reserved dd ?
ends

struct LOOKUP_STREAM_FROM_CLUSTER_INPUT
       Flags dd ?
       NumberOfClusters dd ?
       Cluster dq ?
ends

struct LOOKUP_STREAM_FROM_CLUSTER_OUTPUT
       Offset dd ?
       NumberOfMatches dd ?
       BufferSizeRequired dd ?
ends

LOOKUP_STREAM_FROM_CLUSTER_ENTRY_FLAG_PAGE_FILE = 1
LOOKUP_STREAM_FROM_CLUSTER_ENTRY_FLAG_DENY_DEFRAG_SET = 2
LOOKUP_STREAM_FROM_CLUSTER_ENTRY_FLAG_FS_SYSTEM_FILE = 4
LOOKUP_STREAM_FROM_CLUSTER_ENTRY_FLAG_TXF_SYSTEM_FILE = 8

LOOKUP_STREAM_FROM_CLUSTER_ENTRY_ATTRIBUTE_MASK = $ff000000
LOOKUP_STREAM_FROM_CLUSTER_ENTRY_ATTRIBUTE_DATA = $1000000
LOOKUP_STREAM_FROM_CLUSTER_ENTRY_ATTRIBUTE_INDEX = $2000000
LOOKUP_STREAM_FROM_CLUSTER_ENTRY_ATTRIBUTE_SYSTEM = $3000000

struct LOOKUP_STREAM_FROM_CLUSTER_ENTRY
       OffsetToNext dd ?
       Flags dd ?
       Reserved dq ?
       Cluster dq ?
       FileName dw ?,?,?,?
ends

struct FILE_TYPE_NOTIFICATION_INPUT
       Flags dd ?
       NumFileTypeIDs dd ?
       FileTypeID GUID
ends

FILE_TYPE_NOTIFICATION_FLAG_USAGE_BEGIN = 1
FILE_TYPE_NOTIFICATION_FLAG_USAGE_END = 2

;FILE_TYPE_NOTIFICATION_GUID_PAGE_FILE GUID $d0a64a1,$38fc,$4db8,<$9f,$e7,$3f,$43,$52,$cd,$7c,$5c>
;FILE_TYPE_NOTIFICATION_GUID_HIBERNATION_FILE GUID $b7624d64,$b9a3,$4cf8,<$80,$11,$5b,$86,$c9,$40,$e7,$b7>
;FILE_TYPE_NOTIFICATION_GUID_CRASHDUMP_FILE GUID $9d453eb7,$d2a6,$4dbd,<$a2,$e3,$fb,$d0,$ed,$91,$09,$a9>

IOCTL_VOLUME_BASE = $56

IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS = $560000

struct DISK_EXTENT
       DiskNumber dd ?,?
       StartingOffset dq ?
       ExtentLength dq ?
ends

struct VOLUME_DISK_EXTENTS
       NumberOfDiskExtents dd ?,?
       Extents DISK_EXTENT
ends

IOCTL_VOLUME_ONLINE = $56C008
IOCTL_VOLUME_OFFLINE = $56C00C
IOCTL_VOLUME_IS_CLUSTERED = $560030
IOCTL_VOLUME_GET_GPT_ATTRIBUTES = $560038

struct VOLUME_GET_GPT_ATTRIBUTES_INFORMATION
       GptAttributes dq ?
ends

;Copyright (c) Microsoft Corporation. All rights reserved.
;Module Name: USB.H
;Abstract:    structures and APIs for USB drivers.
;Environment: Kernel & user mode

USBDI_VERSION = $600

BMREQUEST_HOST_TO_DEVICE = 0
BMREQUEST_DEVICE_TO_HOST = 1

BMREQUEST_STANDARD = 0
BMREQUEST_CLASS = 1
BMREQUEST_VENDOR = 2

BMREQUEST_TO_DEVICE = 0
BMREQUEST_TO_INTERFACE = 1
BMREQUEST_TO_ENDPOINT = 2
BMREQUEST_TO_OTHER = 3

MAXIMUM_USB_STRING_LENGTH = 255

USB_GETSTATUS_SELF_POWERED = 1
USB_GETSTATUS_REMOTE_WAKEUP_ENABLED = 2

USB_DEVICE_DESCRIPTOR_TYPE = 1
USB_CONFIGURATION_DESCRIPTOR_TYPE = 2
USB_STRING_DESCRIPTOR_TYPE = 3
USB_INTERFACE_DESCRIPTOR_TYPE = 4
USB_ENDPOINT_DESCRIPTOR_TYPE = 5

USB_RESERVED_DESCRIPTOR_TYPE = 6
USB_CONFIG_POWER_DESCRIPTOR_TYPE = 7
USB_INTERFACE_POWER_DESCRIPTOR_TYPE = 8

macro USB_DESCRIPTOR_MAKE_TYPE_AND_INDEX d, i {
      VALUE = (d shl 8) or i
}

USB_ENDPOINT_TYPE_MASK = 3
USB_ENDPOINT_TYPE_CONTROL = 0
USB_ENDPOINT_TYPE_ISOCHRONOUS = 1
USB_ENDPOINT_TYPE_BULK = 2
USB_ENDPOINT_TYPE_INTERRUPT = 3

USB_CONFIG_POWERED_MASK = $c0
USB_CONFIG_BUS_POWERED = $80
USB_CONFIG_SELF_POWERED = $40
USB_CONFIG_REMOTE_WAKEUP = $20

USB_ENDPOINT_DIRECTION_MASK = $80
;USB_ENDPOINT_DIRECTION_OUT(addr) = (!((addr) & USB_ENDPOINT_DIRECTION_MASK))
;USB_ENDPOINT_DIRECTION_IN(addr)  = ((addr) & USB_ENDPOINT_DIRECTION_MASK)

USB_REQUEST_GET_STATUS = 0
USB_REQUEST_CLEAR_FEATURE = 1
USB_REQUEST_SET_FEATURE = 3
USB_REQUEST_SET_ADDRESS = 5
USB_REQUEST_GET_DESCRIPTOR = 6
USB_REQUEST_SET_DESCRIPTOR = 7
USB_REQUEST_GET_CONFIGURATION = 8
USB_REQUEST_SET_CONFIGURATION = 9
USB_REQUEST_GET_INTERFACE = $A
USB_REQUEST_SET_INTERFACE = $B
USB_REQUEST_SYNC_FRAME = $C

USB_DEVICE_CLASS_RESERVED = 0
USB_DEVICE_CLASS_AUDIO = 1
USB_DEVICE_CLASS_COMMUNICATIONS = 2
USB_DEVICE_CLASS_HUMAN_INTERFACE = 3
USB_DEVICE_CLASS_MONITOR = 4
USB_DEVICE_CLASS_PHYSICAL_INTERFACE = 5
USB_DEVICE_CLASS_POWER = 6
USB_DEVICE_CLASS_PRINTER = 7
USB_DEVICE_CLASS_STORAGE = 8
USB_DEVICE_CLASS_HUB = 9
USB_DEVICE_CLASS_VENDOR_SPECIFIC = $FF

USB_FEATURE_ENDPOINT_STALL = 0
USB_FEATURE_REMOTE_WAKEUP = 1
USB_FEATURE_INTERFACE_POWER_D0 = 2
USB_FEATURE_INTERFACE_POWER_D1 = 3
USB_FEATURE_INTERFACE_POWER_D2 = 4
USB_FEATURE_INTERFACE_POWER_D3 = 5

struct USB_DEVICE_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       bcdUSB dw ?
       bDeviceClass db ?
       bDeviceSubClass db ?
       bDeviceProtocol db ?
       bMaxPacketSize0 db ?
       idVendor dw ?
       idProduct dw ?
       bcdDevice dw ?
       iManufacturer db ?
       iProduct db ?
       iSerialNumber db ?
       bNumConfigurations db ?
ends

struct USB_ENDPOINT_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       bEndpointAddress db ?
       bmAttributes db ?
       wMaxPacketSize dw ?
       bInterval db ?
ends

struct USB_CONFIGURATION_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       wTotalLength dw ?
       bNumInterfaces db ?
       bConfigurationValue db ?
       iConfiguration db ?
       bmAttributes db ?
       MaxPower db ?
ends

struct USB_INTERFACE_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       bInterfaceNumber db ?
       bAlternateSetting db ?
       bNumEndpoints db ?
       bInterfaceClass db ?
       bInterfaceSubClass db ?
       bInterfaceProtocol db ?
       iInterface db ?
ends

struct USB_STRING_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       bString du ?
ends

struct USB_COMMON_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
ends

struct USB_HUB_DESCRIPTOR
       bDescriptorLength db ?
       bDescriptorType db ?
       bNumberOfPorts db ?
       wHubCharacteristics dw ?
       bPowerOnToPowerGood db ?
       bHubControlCurrent db ?
       bRemoveAndPowerMask db 64 dup(?)
ends

USB_SUPPORT_D0_COMMAND = 1
USB_SUPPORT_D1_COMMAND = 2
USB_SUPPORT_D2_COMMAND = 4
USB_SUPPORT_D3_COMMAND = 8
USB_SUPPORT_D1_WAKEUP = $10
USB_SUPPORT_D2_WAKEUP = $20

;right here
struct USB_CONFIGURATION_POWER_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       SelfPowerConsumedD0 db 3 dup(?)
       bPowerSummaryId db ?
       bBusPowerSavingD1 db ?
       bSelfPowerSavingD1 db ?
       bBusPowerSavingD2 db ?
       bSelfPowerSavingD2 db ?
       bBusPowerSavingD3 db ?
       bSelfPowerSavingD3 db ?
       TransitionTimeFromD1 dw ?
       TransitionTimeFromD2 dw ?
       TransitionTimeFromD3 dw ?
ends

struct USB_INTERFACE_POWER_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       bmCapabilitiesFlags db ?
       bBusPowerSavingD1 db ?
       bSelfPowerSavingD1 db ?
       bBusPowerSavingD2 db ?
       bSelfPowerSavingD2 db ?
       bBusPowerSavingD3 db ?
       bSelfPowerSavingD3 db ?
       TransitionTimeFromD1 dw ?
       TransitionTimeFromD2 dw ?
       TransitionTimeFromD3 dw ?
ends

UsbLowSpeed = 0
UsbFullSpeed = 1
UsbHighSpeed = 2

Usb11Device = 0
Usb20Device = 1

USB_PORT_STATUS_CONNECT = 1
USB_PORT_STATUS_ENABLE = 2
USB_PORT_STATUS_SUSPEND = 4
USB_PORT_STATUS_OVER_CURRENT = 8
USB_PORT_STATUS_RESET = $10
USB_PORT_STATUS_POWER = $100
USB_PORT_STATUS_LOW_SPEED = $200
USB_PORT_STATUS_HIGH_SPEED = $400

struct BM_REQUEST_TYPE
       union
	   struct
	       union
		   Recipent db ?
		   Reserved db ?
		   Type db ?
		   Dir db ?
	       ends
	   ends
	   B db ?
       ends
ends

struct USB_DEFAULT_PIPE_SETUP_PACKET
       bmRequestType BM_REQUEST_TYPE
       bRequest db ?
       union
	   struct
	       wValue_LowByte db ?
	       wValue_HiByte db ?
	   ends
	   wValue dw ?
       ends
       union
	   struct
	       wIndex_LowByte db ?
	       wIndex_HiByte db ?
	   ends
	   wIndex dw ?
       ends
       wLength dw ?
ends

USB_DEVICE_QUALIFIER_DESCRIPTOR_TYPE = 6
USB_OTHER_SPEED_CONFIGURATION_DESCRIPTOR_TYPE = 7

struct USB_DEVICE_QUALIFIER_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       bcdUSB dw ?
       bDeviceClass db ?
       bDeviceSubClass db ?
       bDeviceProtocol db ?
       bMaxPacketSize0 db ?
       bNumConfigurations db ?
       bReserved db ?
ends

struct USB_HIGH_SPEED_MAXPACKET
       union
	   struct
	       union
		   MaxPacket dw ?
		   HSmux dw ?
		   Reserved dw ?
	       ends
	   ends
	   us dw ?
       ends
ends

USB_INTERFACE_ASSOCIATION_DESCRIPTOR_TYPE = $B

struct USB_INTERFACE_ASSOCIATION_DESCRIPTOR
       bLength db ?
       bDescriptorType db ?
       bFirstInterface db ?
       bInterfaceCount db ?
       bFunctionClass db ?
       bFunctionSubClass db ?
       bFunctionProtocol db ?
       iFunction db ?
ends

USB_PORTATTR_NO_CONNECTOR = 1
USB_PORTATTR_SHARED_USB2 = 2
USB_PORTATTR_MINI_CONNECTOR = 4
USB_PORTATTR_OEM_CONNECTOR = 8
USB_PORTATTR_OWNED_BY_CC = $1000000
USB_PORTATTR_NO_OVERCURRENT_UI = $2000000

USB_HcGeneric = 0

OHCI_Generic = 100
OHCI_Hydra = 101
OHCI_NEC = 102

UHCI_Generic = 200
UHCI_Piix4 = 201
UHCI_Piix3 = 202
UHCI_Ich2 = 203
UHCI_Reserved204 = 204
UHCI_Ich1 = 205
UHCI_Ich3m = 206
UHCI_Ich4 = 207
UHCI_Ich5 = 208
UHCI_Ich6 = 209

UHCI_Intel = 249

UHCI_VIA = 250
UHCI_VIA_x01 = 251
UHCI_VIA_x02 = 252
UHCI_VIA_x03 = 253
UHCI_VIA_x04 = 254

UHCI_VIA_x0E_FIFO = 264

EHCI_Generic = 1000
EHCI_NEC = 2000
EHCI_Lucent = 3000

USB_DEFAULT_DEVICE_ADDRESS = 0
USB_DEFAULT_ENDPOINT_ADDRESS = 0
USB_DEFAULT_MAX_PACKET = 64

URB_FUNCTION_SELECT_CONFIGURATION = 0
URB_FUNCTION_SELECT_INTERFACE = 1
URB_FUNCTION_ABORT_PIPE = 2
URB_FUNCTION_TAKE_FRAME_LENGTH_CONTROL = 3
URB_FUNCTION_RELEASE_FRAME_LENGTH_CONTROL = 4
URB_FUNCTION_GET_FRAME_LENGTH = 5
URB_FUNCTION_SET_FRAME_LENGTH = 6
URB_FUNCTION_GET_CURRENT_FRAME_NUMBER = 7
URB_FUNCTION_CONTROL_TRANSFER = 8
URB_FUNCTION_BULK_OR_INTERRUPT_TRANSFER = 9
URB_FUNCTION_ISOCH_TRANSFER = $A
URB_FUNCTION_GET_DESCRIPTOR_FROM_DEVICE = $B
URB_FUNCTION_SET_DESCRIPTOR_TO_DEVICE = $C
URB_FUNCTION_SET_FEATURE_TO_DEVICE = $D
URB_FUNCTION_SET_FEATURE_TO_INTERFACE = $E
URB_FUNCTION_SET_FEATURE_TO_ENDPOINT = $F
URB_FUNCTION_CLEAR_FEATURE_TO_DEVICE = $10
URB_FUNCTION_CLEAR_FEATURE_TO_INTERFACE = $11
URB_FUNCTION_CLEAR_FEATURE_TO_ENDPOINT = $12
URB_FUNCTION_GET_STATUS_FROM_DEVICE = $13
URB_FUNCTION_GET_STATUS_FROM_INTERFACE = $14
URB_FUNCTION_GET_STATUS_FROM_ENDPOINT = $15
URB_FUNCTION_RESERVED_0X0016 = $16
URB_FUNCTION_VENDOR_DEVICE = $17
URB_FUNCTION_VENDOR_INTERFACE = $18
URB_FUNCTION_VENDOR_ENDPOINT = $19
URB_FUNCTION_CLASS_DEVICE = $1A
URB_FUNCTION_CLASS_INTERFACE = $1B
URB_FUNCTION_CLASS_ENDPOINT = $1C
URB_FUNCTION_RESERVE_0X001D = $1D
URB_FUNCTION_SYNC_RESET_PIPE_AND_CLEAR_STALL = $1E
URB_FUNCTION_CLASS_OTHER = $1F
URB_FUNCTION_VENDOR_OTHER = $20
URB_FUNCTION_GET_STATUS_FROM_OTHER = $21
URB_FUNCTION_CLEAR_FEATURE_TO_OTHER = $22
URB_FUNCTION_SET_FEATURE_TO_OTHER = $23
URB_FUNCTION_GET_DESCRIPTOR_FROM_ENDPOINT = $24
URB_FUNCTION_SET_DESCRIPTOR_TO_ENDPOINT = $25
URB_FUNCTION_GET_CONFIGURATION = $26
URB_FUNCTION_GET_INTERFACE = $27
URB_FUNCTION_GET_DESCRIPTOR_FROM_INTERFACE = $28
URB_FUNCTION_SET_DESCRIPTOR_TO_INTERFACE = $29

URB_FUNCTION_GET_MS_FEATURE_DESCRIPTOR = $2A
URB_FUNCTION_SYNC_RESET_PIPE = $30
URB_FUNCTION_SYNC_CLEAR_STALL = $31

URB_FUNCTION_CONTROL_TRANSFER_EX = $32
URB_FUNCTION_RESERVE_0X0033 = $33
URB_FUNCTION_RESERVE_0X0034 = $34

URB_FUNCTION_RESERVE_0X002B = $2B
URB_FUNCTION_RESERVE_0X002C = $2C
URB_FUNCTION_RESERVE_0X002D = $2D
URB_FUNCTION_RESERVE_0X002E = $2E
URB_FUNCTION_RESERVE_0X002F = $2F

URB_FUNCTION_RESET_PIPE = URB_FUNCTION_SYNC_RESET_PIPE_AND_CLEAR_STALL

USBD_TRANSFER_DIRECTION = 1
USBD_SHORT_TRANSFER_OK = 2
USBD_START_ISO_TRANSFER_ASAP = 4
USBD_DEFAULT_PIPE_TRANSFER = 8

macro USBD_TRANSFER_DIRECTION_FLAG flags {
      VALUE = (flags & USBD_TRANSFER_DIRECTION)
}

USBD_TRANSFER_DIRECTION_OUT = 0
USBD_TRANSFER_DIRECTION_IN = 1

VALID_TRANSFER_FLAGS_MASK = (USBD_SHORT_TRANSFER_OK or USBD_TRANSFER_DIRECTION or USBD_START_ISO_TRANSFER_ASAP or\
			     USBD_DEFAULT_PIPE_TRANSFER)

USBD_ISO_START_FRAME_RANGE = 1024

USBD_STATUS_SUCCESS = 0
USBD_STATUS_PENDING = $40000000

USBD_STATUS_CRC = $C0000001
USBD_STATUS_BTSTUFF = $C0000002
USBD_STATUS_DATA_TOGGLE_MISMATCH = $C0000003
USBD_STATUS_STALL_PID = $C0000004
USBD_STATUS_DEV_NOT_RESPONDING = $C0000005
USBD_STATUS_PID_CHECK_FAILURE = $C0000006
USBD_STATUS_UNEXPECTED_PID = $C0000007
USBD_STATUS_DATA_OVERRUN = $C0000008
USBD_STATUS_DATA_UNDERRUN = $C0000009
USBD_STATUS_RESERVED1 = $C000000A
USBD_STATUS_RESERVED2 = $C000000B
USBD_STATUS_BUFFER_OVERRUN = $C000000C
USBD_STATUS_BUFFER_UNDERRUN = $C000000D
USBD_STATUS_NOT_ACCESSED = $C000000F
USBD_STATUS_FIFO = $C0000010
USBD_STATUS_XACT_ERROR = $C0000011
USBD_STATUS_BABBLE_DETECTED = $C0000012
USBD_STATUS_DATA_BUFFER_ERROR = $C0000013

USBD_STATUS_ENDPOINT_HALTED = $C0000030
USBD_STATUS_INVALID_URB_FUNCTION = $80000200
USBD_STATUS_INVALID_PARAMETER = $80000300
USBD_STATUS_ERROR_BUSY = $80000400
USBD_STATUS_INVALID_PIPE_HANDLE = $80000600
USBD_STATUS_NO_BANDWIDTH = $80000700
USBD_STATUS_INTERNAL_HC_ERROR = $80000800
USBD_STATUS_ERROR_SHORT_TRANSFER = $80000900
USBD_STATUS_BAD_START_FRAME = $C0000A00
USBD_STATUS_ISOCH_REQUEST_FAILED = $C0000B00
USBD_STATUS_FRAME_CONTROL_OWNED = $C0000C00
USBD_STATUS_FRAME_CONTROL_NOT_OWNED = $C0000D00
USBD_STATUS_NOT_SUPPORTED = $C0000E00
USBD_STATUS_INVALID_CONFIGURATION_DESCRIPTOR = $C0000F00
USBD_STATUS_INSUFFICIENT_RESOURCES = $C0001000
USBD_STATUS_SET_CONFIG_FAILED = $C0002000
USBD_STATUS_BUFFER_TOO_SMALL = $C0003000
USBD_STATUS_INTERFACE_NOT_FOUND = $C0004000
USBD_STATUS_INAVLID_PIPE_FLAGS = $C0005000
USBD_STATUS_TIMEOUT = $C0006000
USBD_STATUS_DEVICE_GONE = $C0007000
USBD_STATUS_STATUS_NOT_MAPPED = $C0008000
USBD_STATUS_HUB_INTERNAL_ERROR = $C0009000
USBD_STATUS_CANCELED = $C0010000
USBD_STATUS_ISO_NOT_ACCESSED_BY_HW = $C0020000
USBD_STATUS_ISO_TD_ERROR = $C0030000
USBD_STATUS_ISO_NA_LATE_USBPORT = $C0040000
USBD_STATUS_ISO_NOT_ACCESSED_LATE = $C0050000

USBD_STATUS_BAD_DESCRIPTOR = $C0100000
USBD_STATUS_BAD_DESCRIPTOR_BLEN = $C0100001
USBD_STATUS_BAD_DESCRIPTOR_TYPE = $C0100002
USBD_STATUS_BAD_INTERFACE_DESCRIPTOR = $C0100003
USBD_STATUS_BAD_ENDPOINT_DESCRIPTOR = $C0100004
USBD_STATUS_BAD_INTERFACE_ASSOC_DESCRIPTOR = $C0100005
USBD_STATUS_BAD_CONFIG_DESC_LENGTH = $C0100006
USBD_STATUS_BAD_NUMBER_OF_INTERFACES = $C0100007
USBD_STATUS_BAD_NUMBER_OF_ENDPOINTS = $C0100008
USBD_STATUS_BAD_ENDPOINT_ADDRESS = $C0100009

USBD_DEFAULT_MAXIMUM_TRANSFER_SIZE = $FFFFFFFF

struct USBD_VERSION_INFORMATION
       USBDI_Version dd ?
       Supported_USB_Version dd ?
ends

UsbdPipeTypeControl = 0
UsbdPipeTypeIsochronous = 1
UsbdPipeTypeBulk = 2
UsbdPipeTypeInterrupt = 3

struct USBD_DEVICE_INFORMATION
       OffsetNext dd ?
       UsbdDeviceHandle dd ?
       DeviceDescriptor USB_DEVICE_DESCRIPTOR
       dw ?
ends

struct USBD_PIPE_INFORMATION
       MaximumPacketSize dw ?
       EndpointAddress db ?
       Interval db ?
       PipeType db ?,?,?,?
       PipeHandle dd ?
       MaximumTransferSize dd ?
       PipeFlags dd ?
ends

USBD_PF_CHANGE_MAX_PACKET = 1
USBD_PF_SHORT_PACKET_OPT = 2
USBD_PF_ENABLE_RT_THREAD_ACCESS = 4
USBD_PF_MAP_ADD_TRANSFERS = 8

USBD_PF_VALID_MASK = (USBD_PF_CHANGE_MAX_PACKET or USBD_PF_SHORT_PACKET_OPT or USBD_PF_ENABLE_RT_THREAD_ACCESS or\
		      USBD_PF_MAP_ADD_TRANSFERS)

struct USBD_INTERFACE_INFORMATION
       Length dw ?
       InterfaceNumber db ?
       AlternateSetting db ?
       Class db ?
       SubClass db ?
       Protocol db ?
       Reserved db ?
       InterfaceHandle dd ?
       NumberOfPipes dd ?
       Pipes USBD_PIPE_INFORMATION
ends

struct _URB_HCD_AREA
       Reserved8 dd 8 dup(?)
ends

struct _URB_HEADER
       Length dw ?
       Function dw ?
       Status dd ?
       UsbdDeviceHandle dd ?
       UsbdFlags dd ?
ends

struct _URB_SELECT_INTERFACE
       Hdr _URB_HEADER
       ConfigurationHandle dd ?
       Interface USBD_INTERFACE_INFORMATION
ends

struct _URB_SELECT_CONFIGURATION
       Hdr _URB_HEADER
       ConfigurationDescriptor dd ?
       ConfigurationHandle dd ?
       Interface USBD_INTERFACE_INFORMATION
ends

struct _URB_PIPE_REQUEST
       Hdr _URB_HEADER
       PipeHandle dd ?
       Reserved dd ?
ends

struct _URB_FRAME_LENGTH_CONTROL
       Hdr _URB_HEADER
ends

struct _URB_GET_FRAME_LENGTH
       Hdr _URB_HEADER
       FrameLength dd ?
       FrameNumber dd ?
ends

struct _URB_SET_FRAME_LENGTH
       Hdr _URB_HEADER
       FrameLengthDelta dd ?
ends

struct _URB_GET_CURRENT_FRAME_NUMBER
       Hdr _URB_HEADER
       FrameNumber dd ?
ends

struct _URB_CONTROL_DESCRIPTOR_REQUEST
       Hdr _URB_HEADER
       Reserved dd ?
       Reserved0 dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       Reserved1 dw ?
       Index db ?
       DescriptorType db ?
       LanguageId dw ?
       Reserved2 dw ?
ends

struct _URB_CONTROL_GET_STATUS_REQUEST
       Hdr _URB_HEADER
       Reserved dd ?
       Reserved0 dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       Reserved1 db 4 dup(?)
       Index dw ?
       Reserved2 dw ?
ends

struct _URB_CONTROL_FEATURE_REQUEST
       Hdr _URB_HEADER
       Reserved dd ?
       Reserved2 dd ?
       Reserved3 dd ?
       Reserved4 dd ?
       Reserved5 dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       Reserved0 dw ?
       FeatureSelector dw ?
       Index dw ?
       Reserved1 dw ?
ends

struct _URB_CONTROL_VENDOR_OR_CLASS_REQUEST
       Hdr _URB_HEADER
       Reserved dd ?
       TransferFlags dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       RequestTypeReservedBits db ?
       Request db ?
       Value dw ?
       Index dw ?
       Reserved1 dw ?
ends

struct _URB_CONTROL_GET_INTERFACE_REQUEST
       Hdr _URB_HEADER
       Reserved dd ?
       Reserved0 dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       Reserved1 db 4 dup(?)
       Interface dw ?
       Reserved2 dw ?
ends

struct _URB_CONTROL_GET_CONFIGURATION_REQUEST
       Hdr _URB_HEADER
       Reserved dd ?
       Reserved0 dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       Reserved1 db 8 dup(?)
ends

OS_STRING_DESCRIPTOR_INDEX = $EE

MS_GENRE_DESCRIPTOR_INDEX = 1
MS_POWER_DESCRIPTOR_INDEX = 2
MS_OS_STRING_SIGNATURE equ "MSFT100"
MS_OS_FLAGS_CONTAINERID = 2

struct OS_STRING
       bLength db ?
       bDescriptorType db ?
       MicrosoftString du 7 dup(?)
       bVendorCode db ?
       union
	   bPad db ?
	   bFlags db ?
       ends
ends

struct _URB_OS_FEATURE_DESCRIPTOR_REQUEST
       Hdr _URB_HEADER
       Reserved dd ?
       Reserved0 dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       union
	   Recipient db ?
	   Reserved1 db ?
       ends
       Reserved2 db ?
       InterfaceNumber db ?
       MS_PageIndex db ?
       MS_FeatureDescriptorIndex dw ?
       Reserved3 dw ?
ends

struct _URB_CONTROL_TRANSFER
       Hdr _URB_HEADER
       PipeHandle dd ?
       TransferFlags dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       SetupPacket db 8 dup(?)
ends

struct _URB_CONTROL_TRANSFER_EX
       Hdr _URB_HEADER
       PipeHandle dd ?
       TransferFlags dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       Timeout dd ?
       hca _URB_HCD_AREA
       SetupPacket db 8 dup(?)
ends

struct _URB_BULK_OR_INTERRUPT_TRANSFER
       Hdr _URB_HEADER
       PipeHandle dd ?
       TransferFlags dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
ends

struct USBD_ISO_PACKET_DESCRIPTOR
       Offset dd ?
       Length dd ?
       Status dd ?
ends

struct _URB_ISOCH_TRANSFER
       Hdr _URB_HEADER
       PipeHandle dd ?
       TransferFlags dd ?
       TransferBufferLength dd ?
       TransferBuffer dd ?
       TransferBufferMDL dd ?
       UrbLink dd ?
       hca _URB_HCD_AREA
       StartFrame dd ?
       NumberOfPackets dd ?
       ErrorCount dd ?
       IsoPacket USBD_ISO_PACKET_DESCRIPTOR
ends

struct URB
       union
	   UrbHeader _URB_HEADER
	   UrbSelectInterface _URB_SELECT_INTERFACE
	   UrbSelectConfiguration _URB_SELECT_CONFIGURATION
	   UrbPipeRequest _URB_PIPE_REQUEST
	   UrbFrameLengthControl _URB_FRAME_LENGTH_CONTROL
	   UrbGetFrameLength _URB_GET_FRAME_LENGTH
	   UrbSetFrameLength _URB_SET_FRAME_LENGTH
	   UrbGetCurrentFrameNumber _URB_GET_CURRENT_FRAME_NUMBER
	   UrbControlTransfer _URB_CONTROL_TRANSFER
	   UrbControlTransferEx _URB_CONTROL_TRANSFER_EX
	   UrbBulkOrInterruptTransfer _URB_BULK_OR_INTERRUPT_TRANSFER
	   UrbIsochronousTransfer _URB_ISOCH_TRANSFER
	   UrbControlDescriptorRequest _URB_CONTROL_DESCRIPTOR_REQUEST
	   UrbControlGetStatusRequest _URB_CONTROL_GET_STATUS_REQUEST
	   UrbControlFeatureRequest _URB_CONTROL_FEATURE_REQUEST
	   UrbControlVendorClassRequest _URB_CONTROL_VENDOR_OR_CLASS_REQUEST
	   UrbControlGetInterfaceRequest _URB_CONTROL_GET_INTERFACE_REQUEST
	   UrbControlGetConfigurationRequest _URB_CONTROL_GET_CONFIGURATION_REQUEST
	   UrbOSFeatureDescriptorRequest _URB_OS_FEATURE_DESCRIPTOR_REQUEST
       ends
ends

;Copyright (c) Microsoft Corporation. All rights reserved.

;Module Name:  USBIOCTL.H
;Abstract:     This file defines both kernel and user mode IOCTL codes supported by the USB core stack.
;Environment:  Kernel & user mode

;Module Name:  USBIODEF.H
;Abstract:     Common header file for all USB IOCTLs defined for the core stack.  We define them in this single header file
;              so that we can maintain backward compatibilty with older versions of the stack.

USB_SUBMIT_URB = 0
USB_RESET_PORT = 1
USB_GET_ROOTHUB_PDO = 3
USB_GET_PORT_STATUS = 4
USB_ENABLE_PORT = 5
USB_GET_HUB_COUNT = 6
USB_CYCLE_PORT = 7
USB_GET_HUB_NAME = 8
USB_IDLE_NOTIFICATION = 9
USB_RECORD_FAILURE = 10
USB_GET_BUS_INFO = 264
USB_GET_CONTROLLER_NAME = 265
USB_GET_BUSGUID_INFO = 266
USB_GET_PARENT_HUB_INFO = 267
USB_GET_DEVICE_HANDLE = 268
USB_GET_DEVICE_HANDLE_EX = 269
USB_GET_TT_DEVICE_HANDLE = 270
USB_GET_TOPOLOGY_ADDRESS = 271
USB_IDLE_NOTIFICATION_EX = 272
USB_REQ_GLOBAL_SUSPEND = 273
USB_REQ_GLOBAL_RESUME = 274
USB_GET_HUB_CONFIG_INFO = 275

HCD_GET_STATS_1 = 255
HCD_DIAGNOSTIC_MODE_ON = 256
HCD_DIAGNOSTIC_MODE_OFF = 257
HCD_GET_ROOT_HUB_NAME = 258
HCD_GET_DRIVERKEY_NAME = 265
HCD_GET_STATS_2 = 266
HCD_DISABLE_PORT = 268
HCD_ENABLE_PORT = 269
HCD_USER_REQUEST = 270
HCD_TRACE_READ_REQUEST = 275

USB_GET_NODE_INFORMATION = 258
USB_GET_NODE_CONNECTION_INFORMATION = 259
USB_GET_DESCRIPTOR_FROM_NODE_CONNECTION = 260
USB_GET_NODE_CONNECTION_NAME = 261
USB_DIAG_IGNORE_HUBS_ON = 262
USB_DIAG_IGNORE_HUBS_OFF = 263
USB_GET_NODE_CONNECTION_DRIVERKEY_NAME = 264
USB_GET_HUB_CAPABILITIES = 271
USB_GET_NODE_CONNECTION_ATTRIBUTES = 272
USB_HUB_CYCLE_PORT = 273
USB_GET_NODE_CONNECTION_INFORMATION_EX = 274
USB_RESET_HUB = 275
USB_GET_HUB_CAPABILITIES_EX = 276
	      
;GUID_DEVINTERFACE_USB_HUB GUID $f18a0e88,$c30c,$11d0,<$88,$15,0,$a0,$c9,$06,$be,$d8>
;GUID_DEVINTERFACE_USB_DEVICE GUID $A5DCBF10L,$6530,$11D2,<$90,$1F,0,$C0,$4F,$B9,$51,$ED>
;GUID_DEVINTERFACE_USB_HOST_CONTROLLER GUID $3abf6f2d,$71c4,$462a,<$8a,$92,$1e,$68,$61,$e6,$af,$27>
;GUID_USB_WMI_STD_DATA GUID $4E623B20L,$CB14,$11D1,<$B3,$31,0,$A0,$C9,$59,$BB,$D2>
;GUID_USB_WMI_STD_NOTIFICATION GUID $4E623B20L,$CB14,$11D1,<$B3,$31,0,$A0,$C9,$59,$BB,$D2>
;GUID_USB_WMI_DEVICE_PERF_INFO GUID $66c1aa3c,$499f,$49a0,<$a9,$a5,$61,$e2,$35,$9f,$64,7>
;GUID_USB_WMI_NODE_INFO GUID $9c179357,$dc7a,$4f41,<$b6,$6b,$32,$3b,$9d,$dc,$b5,$b1>
;GUID_USB_WMI_HUB_DIAGNOSTICS GUID $ad0379e4,$72db,$42ed,<$ba,$6e,$67,$57,$4,$79,$7,$d>
;GUID_USB_WMI_TRACING GUID $3a61881b,$b4e6,$4bf9,<$ae,$f,$3c,$d8,$f3,$94,$e5,$2f>
;GUID_USB_TRANSFER_TRACING GUID $681eb8aa,$403d,$452c,<$9f,$8a,$f0,$61,$6f,$ac,$95,$40>
;GUID_USB_PERFORMANCE_TRACING GUID $d5de77a6,$6ae9,$425c,<$b1,$e2,$f5,$61,$5f,$d3,$48,$a9>

;GUID_CLASS_USBHUB GUID $f18a0e88,$c30c,$11d0,<$88,$15,0,$a0,$c9,$06,$be,$d8>
;GUID_CLASS_USB_DEVICE GUID $A5DCBF10L,$6530,$11D2,<$90,$1F,0,$C0,$4F,$B9,$51,$ED>
;GUID_CLASS_USB_HOST_CONTROLLER GUID $3abf6f2d,$71c4,$462a,<$8a,$92,$1e,$68,$61,$e6,$af,$27>

FILE_DEVICE_USB = FILE_DEVICE_UNKNOWN

struct USB_IDLE_CALLBACK_INFO
       IdleCallback dd ?
       IdleContext dd ?
ends

IOCTL_INTERNAL_USB_SUBMIT_URB = $220003
IOCTL_INTERNAL_USB_RESET_PORT = $220007
IOCTL_INTERNAL_USB_GET_ROOTHUB_PDO = $22000f

USBD_PORT_ENABLED = 1
USBD_PORT_CONNECTED = 2

IOCTL_INTERNAL_USB_GET_PORT_STATUS = $220013
IOCTL_INTERNAL_USB_ENABLE_PORT = $220017
IOCTL_INTERNAL_USB_GET_HUB_COUNT = $22001b
IOCTL_INTERNAL_USB_CYCLE_PORT = $22001f
IOCTL_INTERNAL_USB_GET_HUB_NAME = $220020
IOCTL_INTERNAL_USB_GET_BUS_INFO = $220420
IOCTL_INTERNAL_USB_GET_CONTROLLER_NAME = $220424
IOCTL_INTERNAL_USB_GET_BUSGUID_INFO = $220428
IOCTL_INTERNAL_USB_GET_PARENT_HUB_INFO = $22042c

IOCTL_INTERNAL_USB_SUBMIT_IDLE_NOTIFICATION = $220027
IOCTL_INTERNAL_USB_GET_DEVICE_HANDLE = $220433
IOCTL_INTERNAL_USB_NOTIFY_IDLE_READY = $220443
IOCTL_INTERNAL_USB_REQ_GLOBAL_SUSPEND = $220447
IOCTL_INTERNAL_USB_REQ_GLOBAL_RESUME = $22044b

struct USB_START_FAILDATA
       LengthInBytes dd ?
       NtStatus dd ?
       UsbdStatus dd ?
       ConnectStatus dd ?
       DriverData db ?,?,?,?
ends

IOCTL_INTERNAL_USB_RECORD_FAILURE = $22002b
IOCTL_INTERNAL_USB_GET_DEVICE_HANDLE_EX = $220437
IOCTL_INTERNAL_USB_GET_TT_DEVICE_HANDLE = $22043b

struct USB_TOPOLOGY_ADDRESS
       PciBusNumber dd ?
       PciDeviceNumber dd ?
       PciFunctionNumber dd ?
       Reserved dd ?
       RootHubPortNumber dw ?
       HubPortNumber dw 5 dup(?)
       Reserved2 dw ?
ends
	
IOCTL_INTERNAL_USB_GET_TOPOLOGY_ADDRESS = $22043f
IOCTL_INTERNAL_USB_GET_DEVICE_CONFIG_INFO = $22044f

IOCTL_USB_HCD_GET_STATS_1 = $2203fc
IOCTL_USB_HCD_GET_STATS_2 = $220428
IOCTL_USB_HCD_DISABLE_PORT = $220430
IOCTL_USB_HCD_ENABLE_PORT = $220434
IOCTL_USB_DIAGNOSTIC_MODE_ON = $220400
IOCTL_USB_DIAGNOSTIC_MODE_OFF = $220404
IOCTL_USB_GET_ROOT_HUB_NAME = $220408
IOCTL_GET_HCD_DRIVERKEY_NAME = $220424
IOCTL_USB_GET_NODE_INFORMATION = $220408
IOCTL_USB_GET_NODE_CONNECTION_INFORMATION = $22040c
IOCTL_USB_GET_DESCRIPTOR_FROM_NODE_CONNECTION = $220410
IOCTL_USB_GET_NODE_CONNECTION_NAME = $220414
IOCTL_USB_DIAG_IGNORE_HUBS_ON = $220418
IOCTL_USB_DIAG_IGNORE_HUBS_OFF = $22041c
IOCTL_USB_GET_NODE_CONNECTION_DRIVERKEY_NAME = $220420
IOCTL_USB_GET_HUB_CAPABILITIES = $22043c
IOCTL_USB_HUB_CYCLE_PORT = $220444
IOCTL_USB_GET_NODE_CONNECTION_ATTRIBUTES = $220440
IOCTL_USB_GET_NODE_CONNECTION_INFORMATION_EX = $220448
IOCTL_USB_RESET_HUB = $22044c
IOCTL_USB_GET_HUB_CAPABILITIES_EX = $220450

UsbHub = 0
UsbMIParent = 1

struct USB_HUB_INFORMATION
       HubDescriptor USB_HUB_DESCRIPTOR
       HubIsBusPowered db ?
ends

struct USB_MI_PARENT_INFORMATION
       NumberOfInterfaces dd ?
ends

struct USB_NODE_INFORMATION
       NodeType dd ?
       union
	   HubInformation USB_HUB_INFORMATION
	   MiParentInformation USB_MI_PARENT_INFORMATION
       ends
ends

struct USB_PIPE_INFO
       EndpointDescriptor USB_ENDPOINT_DESCRIPTOR
       ScheduleOffset dd ?
ends

NoDeviceConnected = 0
DeviceConnected = 1
DeviceFailedEnumeration = 2
DeviceGeneralFailure = 3
DeviceCausedOvercurrent = 4
DeviceNotEnoughPower = 5
DeviceNotEnoughBandwidth = 6
DeviceHubNestedTooDeeply = 7
DeviceInLegacyHub = 8
DeviceEnumerating = 9
DeviceReset = 10

struct USB_NODE_CONNECTION_INFORMATION
       ConnectionIndex dd ?
       DeviceDescriptor USB_DEVICE_DESCRIPTOR
       CurrentConfigurationValue db ?
       LowSpeed db ?
       DeviceIsHub db ?
       DeviceAddress dw ?
       NumberOfOpenPipes dd ?
       ConnectionStatus dd ?
       PipeList USB_PIPE_INFO
ends

struct USB_NODE_CONNECTION_DRIVERKEY_NAME
       ConnectionIndex dd ?
       ActualLength dd ?
       DriverKeyName dw ?
ends

struct USB_NODE_CONNECTION_NAME
       ConnectionIndex dd ?
       ActualLength dd ?
       NodeName dw ?
ends

struct USB_HUB_NAME
       ActualLength dd ?
       HubName dw ?
ends

struct USB_ROOT_HUB_NAME
       ActualLength dd ?
       RootHubName dw ?
ends

struct USB_HCD_DRIVERKEY_NAME
       ActualLength dd ?
       DriverKeyName dw ?
ends

struct USB_DESCRIPTOR_REQUEST
       ConnectionIndex dd ?
       struct
	   bmRequest db ?
	   bRequest db ?
	   wValue dw ?
	   wIndex dw ?
	   wLength dw ?
       ends
       Data db ?
ends

struct USB_HUB_CAPABILITIES
       HubIs2xCapable dd ?
ends

struct USB_NODE_CONNECTION_ATTRIBUTES
       ConnectionIndex dd ?
       ConnectionStatus dd ?
       PortAttributes dd ?
ends

struct USB_NODE_CONNECTION_INFORMATION_EX
       ConnectionIndex dd ?
       DeviceDescriptor USB_DEVICE_DESCRIPTOR
       CurrentConfigurationValue db ?
       Speed db ?
       DeviceIsHub db ?
       DeviceAddress dw ?
       NumberOfOpenPipes dd ?
       ConnectionStatus dd ?
       PipeList USB_PIPE_INFO
ends

struct USB_HUB_CAP_FLAGS
       union
	   ul dd ?
	   struct
	       HubIsHighSpeedCapable dd ?
	       HubIsHighSpeed dd ?
	       HubIsMultiTtCapable dd ?
	       HubIsMultiTt dd ?
	       HubIsRoot dd ?
	       HubIsArmedWakeOnConnect dd ?
	       HubIsBusPowered dd ?
	       ReservedMBZ dd ?
	   ends
       ends
ends

struct USB_HUB_CAPABILITIES_EX
       CapabilityFlags USB_HUB_CAP_FLAGS
ends

struct USB_CYCLE_PORT_PARAMS
       ConnectionIndex dd ?
       StatusReturned dd ?
ends

struct USB_ID_STRING
       LanguageId dw ?
       Pad dw ?
       LengthInBytes dd ?
       Buffer dd ?
ends

struct USB_HUB_DEVICE_UXD_SETTINGS
       Version dd ?
       PnpGuid GUID
       OwnerGuid GUID
       DeleteOnShutdown dd ?
       DeleteOnReload dd ?
       DeleteOnDisconnect dd ?
       Reserved dd 5 dup(?)
ends

struct HUB_DEVICE_CONFIG_INFO
       Version dd ?
       Length dd ?
       HubFlags USB_HUB_CAP_FLAGS
       HardwareIds USB_ID_STRING
       CompatibleIds USB_ID_STRING
       DeviceDescription USB_ID_STRING
       Reserved dd 19 dup(?)
       UxdSettings USB_HUB_DEVICE_UXD_SETTINGS
ends

struct HCD_STAT_COUNTERS
       BytesTransferred dd ?
       IsoMissedCount dw ?
       DataOverrunErrorCount dw ?
       CrcErrorCount dw ?
       ScheduleOverrunCount dw ?
       TimeoutErrorCount dw ?
       InternalHcErrorCount dw ?
       BufferOverrunErrorCount dw ?
       SWErrorCount dw ?
       StallPidCount dw ?
       PortDisableCount dw ?
ends

struct HCD_ISO_STAT_COUNTERS
       LateUrbs dw ?
       DoubleBufferedPackets dw ?
       TransfersCF_5ms dw ?
       TransfersCF_2ms dw ?
       TransfersCF_1ms dw ?
       MaxInterruptLatency dw ?
       BadStartFrame dw ?
       StaleUrbs dw ?
       IsoPacketNotAccesed dw ?
       IsoPacketHWError dw ?
       SmallestUrbPacketCount dw ?
       LargestUrbPacketCount dw ?
       IsoCRC_Error dw ?
       IsoOVERRUN_Error dw ?
       IsoINTERNAL_Error dw ?
       IsoUNKNOWN_Error dw ?
       IsoBytesTransferred dd ?
       LateMissedCount dw ?
       HWIsoMissedCount dw ?
       Reserved7 dd 8 dup(?)
ends

struct HCD_STAT_INFORMATION_1
       Reserved1 dd ?
       Reserved2 dd ?
       ResetCounters dd ?
       TimeRead dq ?
       Counters HCD_STAT_COUNTERS
ends

struct HCD_STAT_INFORMATION_2
       Reserved1 dd ?
       Reserved2 dd ?
       ResetCounters dd ?
       TimeRead dq ?
       LockedMemoryUsed dd ?
       Counters HCD_STAT_COUNTERS
       IsoCounters HCD_ISO_STAT_COUNTERS
ends

WMI_USB_DRIVER_INFORMATION = 0
WMI_USB_DRIVER_NOTIFICATION = 1
WMI_USB_POWER_DEVICE_ENABLE = 2
WMI_USB_HUB_NODE_INFORMATION = 4
WMI_USB_PERFORMANCE_INFORMATION = 1
WMI_USB_DEVICE_NODE_INFORMATION = 2

EnumerationFailure = 0
InsufficentBandwidth = 1
InsufficentPower = 2
OverCurrent = 3
ResetOvercurrent = 4
AcquireBusInfo = 5
AcquireHubName = 6
AcquireControllerName = 7
HubOvercurrent = 8
HubPowerChange = 9
HubNestedTooDeeply = 10
ModernDeviceInLegacyHub = 11

struct USB_NOTIFICATION
       NotificationType dd ?
ends

struct USB_CONNECTION_NOTIFICATION
       NotificationType dd ?
       ConnectionNumber dd ?
       RequestedBandwidth dd ?
       EnumerationFailReason dd ?
       PowerRequested dd ?
       HubNameLength dd ?
ends

struct USB_BUS_NOTIFICATION
       NotificationType dd ?
       TotalBandwidth dd ?
       ConsumedBandwidth dd ?
       ControllerNameLength dd ?
ends

struct USB_ACQUIRE_INFO
       NotificationType dd ?
       TotalSize dd ?
       Buffer dw ?
ends

USB_NODE_INFO_SIG equ 'USBN'

UsbDevice = 0
HubDevice = 1
CompositeDevice = 2
UsbController = 3

struct USB_DEVICE_STATE
       union
	   DeviceConnected dd ?
	   DeviceStarted dd ?
       ends
ends

struct USB_HUB_PORT_INFORMATION
       DeviceState USB_DEVICE_STATE
       PortNumber dw ?
       DeviceAddress dw ?
       ConnectionIndex dd ?
       ConnectionStatus dd ?
ends

struct USB_HUB_DEVICE_INFO
       HubDescriptor USB_HUB_DESCRIPTOR
       HubNumber dd ?
       DeviceAddress dw ?
       HubIsSelfPowered db ?
       HubIsRootHub db ?
       HubCapabilities USB_HUB_CAPABILITIES
       NumberOfHubPorts dd ?
       PortInfo USB_HUB_PORT_INFORMATION
ends

struct USB_COMPOSITE_FUNCTION_INFO
       FunctionNumber db ?
       BaseInterfaceNumber db ?
       NumberOfInterfaces db ?
       FunctionIsIdle db ?
ends

struct USB_COMPOSITE_DEVICE_INFO
       DeviceDescriptor USB_DEVICE_DESCRIPTOR
       CurrentConfigDescriptor USB_CONFIGURATION_DESCRIPTOR
       CurrentConfigurationValue db ?
       NumberOfFunctions db ?
       FunctionInfo USB_COMPOSITE_FUNCTION_INFO
ends

struct USB_CONTROLLER_DEVICE_INFO
       PciVendorId dd ?
       PciDeviceId dd ?
       PciRevision dd ?
       NumberOfRootPorts dd ?
       HcFeatureFlags dd ?
ends

struct USB_DEVICE_INFO
       DeviceState USB_DEVICE_STATE
       PortNumber dw ?
       DeviceDescriptor USB_DEVICE_DESCRIPTOR
       CurrentConfigurationValue db ?
       Speed dd ?
       DeviceAddress dw ?
       ConnectionIndex dd ?
       ConnectionStatus dd ?
       PnpHardwareId du 128 dup(?)
       PnpCompatibleId du 128 dup(?)
       SerialNumberId du 128 dup(?)
       PnpDeviceDescription du 128 dup(?)
       NumberOfOpenPipes dd ?
       PipeList USB_PIPE_INFO
ends

struct USB_DEVICE_NODE_INFO
       Sig dd ?
       LengthInBytes dd ?
       DeviceDescription du 40 dup(?)
       NodeType dd ?
       BusAddress USB_TOPOLOGY_ADDRESS
       union
	   UsbDeviceInfo USB_DEVICE_INFO
	   HubDeviceInfo USB_HUB_DEVICE_INFO
	   CompositeDeviceInfo USB_COMPOSITE_FUNCTION_INFO
	   ControllerDeviceInfo USB_CONTROLLER_DEVICE_INFO
	   DeviceInformation db 4 dup(?)
       ends
ends

struct USB_DEVICE_PERFORMANCE_INFO
       BulkBytes dd ?
       ControlDataBytes dd ?
       IsoBytes dd ?
       InterruptBytes dd ?
       BulkUrbCount dd ?
       ControlUrbCount dd ?
       IsoUrbCount dd ?
       InterruptUrbCount dd ?
       AllocedInterrupt dd 6 dup(?)
       AllocedIso dd ?
       Total32secBandwidth dd ?
       TotalTtBandwidth dd ?
       DeviceDescription du 60 dup(?)
       DeviceSpeed dd ?
       TotalIsoLatency dd ?
       DroppedIsoPackets dd ?
       TransferErrors dd ?
       PciInterruptCount dd ?
       HcIdleState dd ?
       HcAsyncIdleState dd ?
       HcAsyncCacheFlushCount dd ?
       HcPeriodicIdleState dd ?
       HcPeriodicCacheFlushCount dd ?
ends
