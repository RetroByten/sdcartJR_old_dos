STRUC request_header
	.length: resb 1
	.unit_code: resb 1
	.command: resb 1
	.status: resw 1
	.reserved: resb 8
	.size:
ENDSTRUC

%define RQ_STATUS_BIT_ERR	0x8000
%define RQ_STATUS_BIT_BUSY	0x0200
%define RQ_STATUS_BIT_DONE	0x0100
%define RQ_STATUS_ERR_WP_VIOLATION		0x0
%define RQ_STATUS_ERR_UNKNOWN_UNIT		0x1
%define RQ_STATUS_ERR_DRIVE_NOT_READY	0x2
%define RQ_STATUS_ERR_UNKNOWN_COMMAND	0x3
%define RQ_STATUS_ERR_CRC_ERROR			0x4
%define RQ_STATUS_ERR_BAD_REQUEST		0x5
%define RQ_STATUS_ERR_SEEK_ERROR		0x6
%define RQ_STATUS_ERR_UNKNOWN_MEDIA		0x7
%define RQ_STATUS_ERR_SECTOR_NOT_FOUND	0x8
%define RQ_STATUS_ERR_OUT_OF_PAPER		0x9
%define RQ_STATUS_ERR_WRITE_FAULT		0xA
%define RQ_STATUS_ERR_READ_FAULT		0xB
%define RQ_STATUS_ERR_GENERAL_FAILURE	0xC

%define CMD_INIT			0x0
%define CMD_MEDIA_CHECK		0x1
%define CMD_BUILD_BPB		0x2
%define CMD_IOCTL_INPUT		0x3
%define CMD_INPUT			0x4
%define CMD_ND_INPUT_NOWAIT	0x5
%define CMD_INPUT_STATUS	0x6
%define CMD_INPUT_FLUSH		0x7
%define CMD_OUTPUT			0x8
%define CMD_OUTPUT_VERIFY	0x9
%define CMD_OUTPUT_STATUS	0xA
%define CMD_OUTPUT_FLUSH	0xB
%define CMD_IOCTL_OUTPUT	0xC

STRUC init_request
	.header: resb request_header.size
	.num_units: resb 1 ; Number of units
	.break_address: resd 1 ; Break address
	.bpb_ptr: resd 1; Also addr of DEVICE= line in CONFIG.SYS (3.0+)
	.bDrvNo: resb 1 ; DOS 3.0+ first drive number assigned to this device
ENDSTRUC

STRUC media_check
	.header: resb request_header.size
	.media_descriptor: resb 1
	.return: resb 1
	; DOS 3+
	.pfVolId: resw 2
ENDSTRUC

%define MEDIA_CHECK_RET_CHANGED		0xff
%define MEDIA_CHECK_RET_NOT_CHANGED	1
%define MEDIA_CHECK_RET_UNKNOWN		0

STRUC rwcommand
	.header: resb request_header.size
	.media_descriptor: resb 1
	.addr: resd 1	; Transfer address
	.count: resw 1	; Sector count
	.start: resw 1	; Starting sector
	; Extended request
	.pfVolId: resw 2 ; Output: addr of Volume ID
	.lBigSect: resw 2 ; 32-bit sector (when .start = 0xffff)
ENDSTRUC

STRUC buildbpb
	.header: resb request_header.size
	.media_descriptor: resb 1
	.addr: resd 1
	.bpb: resd 1	; Pointer to BPB set by the driver
ENDSTRUC

