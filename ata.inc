;
; Disable HDD APM tray version. Stops your drive from clicking (excessive drive head parking)
;
;    Copyright (C) 2012 Gilyazov Rustam <gilyazov@live.com>
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.

;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.


BROADCAST_QUERY_DENY	   = $424D5144
PBT_APMQUERYSUSPEND	   = $00
WM_POWERBROADCAST	   = $218
PBT_APMBATTERYLOW	   = $09
PBT_APMPOWERSTATUSCHANGE   = $0A
PBT_APMOEMEVENT 	   = $0B
PBT_APMQUERYSUSPEND	   = $00
PBT_APMQUERYSUSPENDFAILED  = $02
PBT_APMRESUMECRITICAL	   = $06
PBT_APMRESUMEAUTOMATIC	   = $12
PBT_APMQUERYSUSPENDFAILED  = $02

SMART_GET_VERSION = $74080
SMART_SEND_DRIVE_COMMAND = $7C084
SMART_RCV_DRIVE_DATA = $7C088


struct IDENTIFY_DEVICE_DATA
	GeneralConfiguration		dw ?
	NumCylinders			dw ?
	ReservedWord2			dw ?
	NumHeads			dw ?
	Retired1			dw 2 dup (?)
	NumSectorsPerTrack		dw ?
	VendorUnique1			dw 3 dup (?)
	SerialNumber			db 20 dup (?)
	Retired2			dw 2 dup (?)
	Obsolete1			dw ?
	FirmwareRevision		db 8 dup (?)
	ModelNumber			db 40 dup (?)
	MaximumBlockTransfer		db ?
	VendorUnique2			db ?
	ReservedWord48			dw ?
	Capabilities			dd ?
	ObsoleteWords51 		dw 2 dup (?)
	TranslationFieldsValid		dw ?
	NumberOfCurrentCylinders	dw ?
	NumberOfCurrentHeads		dw ?
	CurrentSectorsPerTrack		dw ?
	CurrentSectorCapacity		dd ?
	CurrentMultiSectorSetting	db ?
	MultiSectorSettingValid 	db ?
	UserAddressableSectors		dd ?
	ObsoleteWord62			dw ?
	MultiWordDMASupport		db ?
	MultiWordDMAActive		db ?
	AdvancedPIOModes		db ?
	ReservedByte64			db ?
	MinimumMWXferCycleTime		dw ?
	RecommendedMWXferCycleTime	dw ?
	MinimumPIOCycleTime		dw ?
	MinimumPIOCycleTimeIORDY	dw ?
	ReservedWords69 		dw 6 dup (?)
	QueueDepth			dw ?
	ReservedWords76 		dw 4 dup (?)
	MajorRevision			dw ?
	MinorRevision			dw ?	
	CommandSetSupport		dw 3 dup (?)
	CommandSetActive		dw 3 dup (?)
	UltraDMASupport 		db ?
	UltraDMAActive			db ?
	ReservedWord89			dw 4 dup (?)
	HardwareResetResult		dw ?
	CurrentAcousticValue		db ?
	RecommendedAcousticValue	db ?
	ReservedWord95			dw 5 dup (?)
	Max48BitLBA			dd 2 dup (?)
	StreamingTransferTime		dw ?
	ReservedWord105 		dw ?
	PhysicalLogicalSectorSize	dw ?
	InterSeekDelay			dw ?
	WorldWideName			dw 4 dup (?)
	ReservedForWorldWideName128	dw 4 dup (?)
	ReservedForTlcTechnicalReport	dw ?
	WordsPerLogicalSector		dw 2 dup (?)
	CommandSetSupportExt		dw ?
	CommandSetActiveExt		dw ?
	ReservedForExpandedSupportandActive	dw 6 dup (?)
	MsnSupport			dw ?
	SecurityStatus			dw ?
	ReservedWord129 		dw 31 dup (?)
	CfaPowerModel			dw ?
	ReservedForCfaWord161		dw 8 dup (?)
	DataSetManagementFeature	dw ?
	ReservedForCfaWord170		dw 6 dup (?)
	CurrentMediaSerialNumber	dw 30 dup (?)
	ReservedWord206 		dw ?
	ReservedWord207 		dw 2 dup (?)
	BlockAlignment			dw ?
	WriteReadVerifySectorCountMode3Only	dw 2 dup (?)
	WriteReadVerifySectorCountMode2Only	dw 2 dup (?)
	NVCacheCapabilities		dw ?
	NVCacheSizeLSW			dw ?
	NVCacheSizeMSW			dw ?
	NominalMediaRotationRate	dw ?
	ReservedWord218 		dw ?
	NVCacheEstimatedTimeToSpinUpInSeconds	dw ?
	Reserved			dw ?
	ReservedWord220 		dw 35 dup (?)
	Signature			db ?
	CheckSum			db ?
ends

IOCTL_IDE_PASS_THROUGH	   = $04D028
IOCTL_ATA_PASS_THROUGH	   = $04D02C
HDIO_DRIVE_CMD		   = $03F1   ; Taken from /usr/include/linux/hdreg.h
HDIO_SET_ACOUSTIC	   = $032C
HDIO_GET_ACOUSTIC	   = $030F
WIN_SETFEATURES 	   = $EF
SETFEATURES_EN_AAM	   = $42
SETFEATURES_DIS_AAM	   = $C2
SETFEATURES_EN_APM	   = $05
SETFEATURES_DIS_APM	   = $85
ATA_FLAGS_DRDY_REQUIRED    = $01
ATA_FLAGS_DATA_IN	   = $02
ATA_FLAGS_DATA_OUT	   = $04
ATA_FLAGS_48BIT_COMMAND    = $08

DRIVE_HEAD_REG	= 0xA0

struct ATA_PASS_THROUGH_EX
	Length			dw ?
	AtaFlags		dw ?
	PathId			db ?
	TargetId		db ?
	Lun			db ?
	ReservedAsUchar 	db ?
	DataTransferLength	dd ?
	TimeOutValue		dd ?
	ReservedAsUlong 	dd ?
	DataBufferOffset	dd ?
	PreviousTaskFile	IDEREGS
	CurrentTaskFile 	IDEREGS
ends
	
struct ATA_PASS_THROUGH_EX_WITH_BUFFERS
	Length			dw ?
	AtaFlags		dw ?
	PathId			db ?
	TargetId		db ?
	Lun			db ?
	ReservedAsUchar 	db ?
	DataTransferLength	dd ?
	TimeOutValue		dd ?
	ReservedAsUlong 	dd ?
	DataBufferOffset	dd ?
	PreviousTaskFile	IDEREGS
	CurrentTaskFile 	IDEREGS
	Filler			db ?
	ucDataBuf		db 512 dup (?)
ends

struct SMARTINFO
    Unknown0		db ?
    AttribIndex 	db ?
    Unknown1		dw ?
    Value		db ?
    Worst		db ?
    Data		dd ?
    Threshold		dw ?
ends
