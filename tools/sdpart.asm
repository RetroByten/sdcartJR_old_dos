; SD-Cart JR : PCJR Card reader cartridge
; Copyright (C) 2020 Raphael Assenat <raph@raphnet.net>
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File sdpart.asm
;;;
;;; Tool for displaying card and partition information.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bits 16
cpu 8086
org 100h

; This must come first, before other code or included code!
entry:
	jmp main

%include 'strutil.asm'
%include 'hexdump.asm'
%include 'print16.asm'

;%define NO_UNROLL_READ512
%include 'spi.asm'


; card_io.asm requires some macros to store information
; about the card type and features. How those are stored
; is application specific. In this case, ds:card_flags is used.
%macro JMP_CARD_IO_FLAG_SET 2
test byte [card_flags], %1
jnz %2
%endmacro
%macro SET_CARD_IO_FLAG 1
or byte [card_flags], %1
%endmacro
%macro CLR_CARD_IO_FLAGS 0
mov byte [card_flags], 0
%endmacro


%undef TRACE_ERRORS
%include 'card_io.asm'

;%define TRACE_CARD_INIT
%include 'card_cmd.asm'


%include 'mbr.asm'

; Fixed disk geometry for chs2lba
%define GEO_SECTORS_PER_TRACK	63
%define GEO_HEADS				16
%include 'chs2lba.asm'

%include 'chsfitter.asm'

main:
	call printBanner
	call readAndPrintDosVersion
	call newline


	printString "Initializing card... "

	call card_init
	jc .init_failed
	printStringLn "OK"


	; Read boot sector
	mov bp, mbrbuf
	call mbr_read
	jc .boot_read_failed

	; This must run once to compute things like
	; card_total_blocks - useful later.
	call readAndPrintCardInfo

	call newline

.prompt:
	call prompt

	cmp al, 27	; ESC
	je exit
	cmp al, 'q'
	je exit
	cmp al, '?'
	je .cmd_help
	cmp al, 'w'
	je .cmd_w
	cmp al, 'p'
	je .cmd_p
	cmp al, 'c'
	je .cmd_c
	cmp al, 'i'
	je .cmd_i
	cmp al, '1'
	je .cmd_1
	cmp al, 'f'
	je .cmd_f

	jmp .prompt


.cmd_help:
	call cmd_help
	jmp .prompt
.cmd_p:
	call cmd_printPartInfo
	jmp .prompt
.cmd_w:
	call cmd_write  ; Write..
	jmp exit        ; and quit.
.cmd_c:
	call cmd_clear
	jmp .prompt
.cmd_i:
	call cmd_printCardInfo
	jmp .prompt

.cmd_1:
	call cmd_create_max32m_part
	jmp .prompt

.cmd_f:
	call cmd_findFirst
	jmp .prompt


.boot_read_failed:
	printString "Error reading boot secotr"
	jmp exit

.init_failed:
	printString "Init failed"
	jmp exit


exit:
	mov ax, 4C00h
	int 21h


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; prompt: Display prompt and read character
	;
	; Returns character in AL
	;
	;
prompt:
	call newline
	printString "Command (? for help): "
	mov ah, 0x01
	int 21h
	call newline
	call newline
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; cmd_printPartInfo: Handler for the 'p' command (print info)
	;
cmd_printPartInfo:
	call displayInfo
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; cmd_printCardInfo: Print Card information
	;
cmd_printCardInfo:
	; Display card information
	call readAndPrintCardInfo
	jc .carderror
	ret
.carderror:
	printString "Card error"
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; cmd_write: Write MBR to disk
	;
cmd_write:
	mov ax, ds
	mov es, ax     ; Source segment
	mov si, mbrbuf ; Source offset
	mov ax, 0      ; Block number (bits 31-16) -> 0
	mov bx, 0      ; Block number (bits 15-0) ->  0
	mov cx, 1      ; Block count (1)
	call card_writeSectors
	jc .write_error
	ret

.write_error:
	printStringLn "Write error"
	stc
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; cmd_clear: Make a new MBR without partitions
	;
cmd_clear:
	mov ax, ds
	mov es, ax
	mov si, default_mbr
	mov di, mbrbuf
	mov cx, 512
	rep movsb

	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; cmd_findFirst: Find the first partition and display its offset.
	;
	; This was for testing before integration into sdcart.sys
	;
cmd_findFirst:
	mov bp, tmpbuf
	call mbr_read
	jc .read_error

	mov si, tmpbuf
	call mbr_getPartitionOffset
	jc .not_found

	printString "Type: "
	call printHexByte
	call newline

	mov dx, ax
	call printHexWord
	mov dx, bx
	call printHexWord
	call newline

	jmp .done

.not_found:
	printStringLn "No suitable partitions found"
	jmp .done

.read_error:
	printStringLn "Error reading MBR"

.done:
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; cmd_create_max32m_part: Make a FAT16<32M partition, as big as possible.
	;
cmd_create_max32m_part:

	printString "Computing last sector... "

	; geo2block requires DS:SI pointing to two words: The number of heads,
	; followed by the number of sectors per track.
	;
	; This fake geometry will have been determined by the chs_fit call already
	; and be stored in a STRUC disk_geometry.
	;
	mov si, geometry + disk_geometry.heads

	; First sector is the next track after the MBR
	mov dh, 0	; Head 0
	mov cx, 0x0101	; CX is as int13. Start at sector 1, track 1
	call geo2block	; Convert the current CHS to a logical sector.
	mov [first_sector], bx
	mov [first_sector+2], ax

.loop:
	call geo2block	; Convert the current CHS to a logical sector.
	; returns 32-bit value in AH AL BH BL
	;                         31........0

	; Check if this sector is still within the card and 64k size constraint
	and ax, ax	; MSW will be non-zero past 64k
	jnz .enough
	; If card has more than 64k sectors, don't bother comparing size
	test word [card_total_blocks+2], 0xffff
	jnz .skip_card_size
	cmp bx, [card_total_blocks]
	jge .enough

.skip_card_size:

	; Still fits? Keep it.
	mov [last_sector], bx
	mov [last_sector+2], ax
	mov [last_sect_cyl], cx

	; And advance one cylinder (TODO : This could be 1 sector..)
	add cx, 0x0100
	jc .inc_high
	jmp .loop
.inc_high:
	add cx, 0x04
	jmp .loop

.enough:

	push dx
		printString "0x"
		mov dx, [last_sector+2]
		call printHexWord
		mov dx, [last_sector]
		call printHexWord
		call newline
	pop dx


	call getFirstFreeOnlyPart
	jc .no_free

	printStringLn "Writing partition entry..."

	mov byte [si+0], 0x80	; Make it active
	mov byte [si+1], 0	; Head 0
	mov byte [si+2], 0x01	; Sector 1
	mov byte [si+3], 1		; Cyl. 1
	mov byte [si+4], 0x04	; FAT16 < 32M
	mov [si+5], dh			; Head
	mov cx, [last_sect_cyl]
	mov [si+6], cl
	mov [si+7], ch
	; LBA first sector
	mov cx, [first_sector]
	mov [si+8], cx
	mov cx, [first_sector+2]
	mov [si+8+2], cx
	; LBA nunmber of sectors
	; Number of sectors. <64k assumed
	mov cx, [last_sector]
	sub cx, [first_sector]
	inc cx
	mov [si+0x0C], cx
	mov word [si+0x0C+2], 0

	jmp .done

.no_free:
	printStringLn "This command only supports creating one partition on the card. Sorry."

.done:
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; cmd_help: Display help
	;
cmd_help:
	printStringLn "Help:"
	call newline
	printStringLn "  ?   Displays help"
	printStringLn "  p   Print partition information"
	printStringLn "  i   Display card information"
	printStringLn "  f   Find first partition offset"
	printStringLn "  c   Make a new MBR without partitions"
	printStringLn "  1   Create FAT16 partition, autosized"
	printStringLn "      to card capacity, but up to. 32MB"
	printStringLn "  w   Write changes to card and quit"
	printStringLn "  q   Quit"

	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; displayInfo: Display some information about the boot sector
	;
	; Assumes boot sector data already in mbrbuf
	;
	;
displayInfo:
	push ax
	push bx
	push cx
	push dx
	push si

	mov bx, mbrbuf

	; Check signature
	mov ax, [bx + 0x1FE]
	cmp ax, 0xAA55
	jne .nosig

	; Display info on each partition
	mov cx, 4
	mov si, mbrbuf
	add si, 0x1Be	; first partition
	mov dl, 1		; partition counter for display

.next_partition:

	printString "Part. "
	call printHexNibble	; Prints value in DL
	printString " : "

	call printPartitionInfo ; Prints info from DS:SI

	call newline

	add si, 16
	inc dl
	loop .next_partition


	jmp .done

.nosig:
	printStringLn "Signature (55AA) not found"

.done:

	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; printPartitionInfo: Displays info about a specific partition (DS:SI)
	;
printPartitionInfo:
	push ax
	push bx
	push cx
	push dx

	;	Status
	mov dl, [si + 0]
	and dl, 0x80
	jnz .active
	and dl, dl	; 0x01-0x7F means invalid
	jnz .invalid
	printString "inactive, "
	jmp .status_done
.invalid:
	printString "invalid, "
	jmp .status_done
.active:
	printString "active, "
.status_done:

	;	Type
	mov dl, [si + 4]
	mov [part_type], dl
	printString "type="
	call printHexByte
	call printPartName ; Displays type in DL inside ()

	and dl, dl
	jz .empty

	printString ", "
	call newline

	;; Display range according to CHS fields ;;
	printString "    CHS: "

	; Cylinder
	mov cl, [si + 3] ; Bits 7-0
	mov ch, [si + 2] ; Bits 9-8 (at bits 7-6)
	rol ch, 1
	rol ch, 1
	and ch, 3 ; Clear sector bits
	call printInt16
	printString ","
	; Head
	mov cl, [si + 1]
	xor ch, ch
	call printInt16
	printString ","
	; Sector
	mov cl, [si + 2]
	and cx, 0x3f	; Keep only sector bits (5-0)
	call printInt16

	printString "-"

	; Cylinder
	mov cl, [si + 3 + 4] ; Bits 7-0
	mov ch, [si + 2 + 4] ; Bits 9-8 (at bits 7-6)
	rol ch, 1
	rol ch, 1
	and ch, 3 ; Clear sector bits
	call printInt16
	printString ","
	; Head
	mov cl, [si + 1 + 4]
	xor ch, ch
	call printInt16
	printString ","
	; Sector
	mov cl, [si + 2 + 4]
	and cx, 0x3f	; Keep only sector bits (5-0)
	call printInt16

	;; Convert those CHS figures to LBA using
	; the SD-Cart geometry.

	;	Range
	printString " ("

	mov dh, [si + 1]	; Head
	mov cx, [si + 2]	; Cylinder and sector
	push si
	; geo2block requires DS:SI pointing to two words: The number of heads,
	; followed by the number of sectors per track.
	mov si, geometry + disk_geometry.heads
	call geo2block
	pop si
	mov dx, ax
	call printHexWord
	mov dx, bx
	call printHexWord

	printString "-"

	mov dh, [si + 5]	; Head
	mov cx, [si + 6]	; Cylinder and sector
	push si
	; geo2block requires DS:SI pointing to two words: The number of heads,
	; followed by the number of sectors per track.
	mov si, geometry + disk_geometry.heads
	call geo2block
	pop si
	mov dx, ax
	call printHexWord
	mov dx, bx
	call printHexWord

	printStringLn ")"

	;; Display range according to LBA fields
	printString "    LBA: "

	;	Range
	printString "blocks "

	mov ax, [si + 0x08 + 2]	; Most significant word in AX
	mov dx, ax
	call printHexWord
	mov bx, [si + 0x08]		; Least significant word in BX
	mov dx, bx
	call printHexWord

	printString "-"

	; Field is number of sectors in partition
	add bx, [si + 0x0C]
	adc ax, [si + 0x0C + 2]
	sub bx, 1	; -1 to display last sector
	sbb ax, 0

	mov dx, ax
	call printHexWord
	mov dx, bx
	call printHexWord

	call  newline

	;; Diagnostics / Comments ;;

	; 1. Check if supported by this DOS version
	mov al, [part_type]
	call checkSupportedByDosVersion
	jc .not_supported
	jmp .supported
.not_supported:
	printStringLn "     * * Not usable by this DOS * *"
.supported:

	; 2. Check if LBA and CHS block match



.empty:

	pop dx
	pop cx
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; printPartName: Display the partition type based on the ID in DL.
	;
	; Does not output a newline.
	;
printPartName:
	and dl,dl
	jz .free
	cmp dl, 0x01
	je .fat12
	cmp dl, 0x04
	je .fat16_32mb
	cmp dl, 0x05
	je .extended_chs
	cmp dl, 0x06
	je .fat16b

	cmp dl, 0x0b
	je .fat32_chs
	cmp dl, 0x0c
	je .fat32_lba
	cmp dl, 0x0e
	je .fat16b_lba
	cmp dl, 0x0f
	je .extended_lba

	jmp .unknown

.extended_lba:
	printString " (Extended, LBA)"
	jmp .done
.fat16b_lba:
	printString " (FAT16B, LBA)"
	jmp .done
.fat32_lba:
	printString " (FAT32, LBA)"
	jmp .done
.fat32_chs:
	printString " (FAT32, CHS)"
	jmp .done
.fat16b:
	printString " (FAT16B)"
	jmp .done
.extended_chs:
	printString " (Extended, CHS)"
	jmp .done
.fat16_32mb:
	printString " (FAT16)"
	jmp .done
.fat12:
	printString " (FAT12)"
	jmp .done
.free:
	printString " (empty)"
	jmp .done
.unknown:
	printString " (unknown)"

.done:
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_init_retry_delay : Delay for retries in card_init
	;
card_init_retry_delay:
	push ax
	push bx
	push cx
	push dx

	mov ah, 00
	int 0x1A
	mov bx, dx	; Keep low order tick count

.lp:
	mov ah, 00
	int 0x1A
	sub dx, bx	; DX = DX - BX
	cmp dx, 2
	jl .lp

	pop dx
	pop cx
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; readAndPrintDosVersion : Get dos version, print and store it
	;
	; Saves it in [dos_major] and [dos_minor]
	;
readAndPrintDosVersion:
	push ax
	push cx
	push dx

	; Get DOS version
	mov ah, 0x30
	int 21h
	mov [dos_major], al
	mov [dos_minor], ah
	printString "Running on DOS "
	xor cx, cx
	mov cl, [dos_major]
	call printInt16
	mov dl, '.'
	call putchar
	mov cl, [dos_minor]
	call printInt16
	call newline

	pop dx
	pop cx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; printBanner : Print a banner with the tool name and/or version
	;
printBanner:
	printStringLn "SDpart version 0.2"
	printStringLn "Partition tool for SD-Cart JR"
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; checkSupportedByDosVersion : Check if the partition type in AL is supported by this DOS version.
	;
	; Returns with CF set if unsupported
	;
checkSupportedByDosVersion:
	cmp byte [dos_major], 2
	jl .not_supported

	cmp al, 0x01	; FAT12
	je .supported

	; DOS 2+ only supports FAT12
	cmp byte [dos_major], 2
	jle .not_supported

	cmp al, 0x04	; FAT16
	je .supported

	; FAT16B supported after DOS 3.31
	cmp byte [dos_major], 3
	je .is_dos_3
	jmp .not_dos_3
.is_dos_3:
	cmp byte [dos_minor], 31
	jl .not_supported
.not_dos_3:
	; DOS 3.31 or 4+
	cmp al, 0x06	; FAT16B
	je .supported

	; Other types are not supported

.not_supported:
	stc
	ret

.supported:
	clc
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; readAndPrintCardInfo: Print stuff from CSD and CID registers
	;
	;
readAndPrintCardInfo:
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	push es
	push bp

	mov ax, ds
	mov es, ax

	; 0  MID : 1 bytes Binary
	; 1  OID : 2 bytes ASCII
	; 3  PNM : 5 bytes ASCII
	; 8  PRV : 1 bytes BCD n.m
	; 9  PSN : 4 bytes Binary
	; 13 MDT : 2 bytes Hex Digits
	; 15 CRC : 1 byte

.read_cid:
	mov di, card_cid
	call card_readCID
	jc .cid_err

	xor dh, dh
	mov dl, [card_cid + 0] ; MID
	printString "Card: "
	call printHexByte

	printString "/"
	mov dx, card_cid + 1
	mov cx, 2
	call putstring_n

	mov al, [card_cid + 0]
	printString " ("
	call printMID
	printString ") "

	;printString "PNM: "
	mov dx, card_cid + 3
	mov cx, 5
	call putstring_n


	printString " v"
	mov al, [card_cid + 8]
	mov cx, 4
	shr al, cl
	xor ch, ch
	mov cl, al
	call printInt16
	printString "."
	mov cl, [card_cid + 8]
	and cl, 0x0f
	call printInt16

	call newline

%if 0
	printString "      CID: "
	mov bp, card_cid
	mov cx, 16
	call hexdump
%endif


	printString "      Serial "
	mov bp, card_cid + 9
	mov cx, 4
	call hexdump

	call newline

	; CSD-slice		MMC/SDV1		SDV2
	;	[6]
	; 		9 READ_BL_PARTIAL
	;		8 WRITE_BLK_MISALIGN
	;		7 READ_BLK_MISALIGN
	;		6 DSR_IMP
	;   	5:74 RESERVED
	;   	73 Device size
	;   	72 Device size
	;	[7]
	;   	71 Device size
	;   	70 Device size
	;   	69 Device size			Device_size
	;   	68 Device size			Device_size
	;   	67 Device size			Device_size
	;   	66 Device size			Device_size
	;   	65 Device size			Device_size
	;   	64 Device size			Device_size
	;	[8]
	;   	63 Device size			Device_size
	;   	62 Device size			Device_size
	;		61:59 VDD_R_CURR_MIN	Device_size
	;		58:56 VDD_R_CURR_MAX	Device_size
	;   [9]
	;		55:53 VDD_W_CURR_MIN	Device_size
	;		52:50 VDD_W_CURR_MAX	Device_size
	;       49 C_SIZE_MULT			Device_size
	;       48 C_SIZE_MULA			Device_size
	;	[10]
	;       47 C_SIZE_MULT
	;		46 ERASE_BLK_EN
	;       45 SECTOR_SIZE
	;       44 SECTOR_SIZE
	;       43 SECTOR_SIZE
	;       42 SECTOR_SIZE
	;       41 SECTOR_SIZE
	;       40 SECTOR_SIZE
	;   [11]
	;       39 SECTOR_SIZE
	;       38:32 WP_GRP_SIZE
	;   [12]
	;       31 WP_GRP_ENABLE
	;       30:29 RESERVED
	;       28:26 R2W_FACTOR
	;       25 WR_BL_LEN
	;		24 WR_BL_LEN
	;   [13]
	;		23 WR_BL_LEN
	;		22 WR_BL_LEN
	;
	; Device size = (csd[8]>>6) | (csd[7]<<2) | (csd[6]) << 12)
	; size mult = (csd[10]>>7) | (csd[9]<<1)
	; block len = (csd[13]>>6) | (csd[12]&3<<2)


.read_csd:
	mov di, card_csd
	call card_readCSD
	jc .csd_err

%if 0
	printString "      CSD: "
	mov bp, card_csd
	mov cx, 16
	call hexdump
%endif

	; [0]
	;   SD                  MMC
	;   127 CSD_STRUCTURE   CSD_STRUCTURE
	;   126 CSD_STRUCTURE	CSD_STRUCTURE
	;   125 reserved        SPEC_VERS
	;   124 reserved        SPEC_VERS
	;   123 reserved        SPEC_VERS
	;   122 reserved        SPEC_VERS
	;   121 reserved        reserved
	;	120	reserved        reserved
	mov al, [card_csd + 0]
	and al, 0xC0
	rol al, 1
	rol al, 1
	; Values for CSD Structure
	; 0 : SD V1  or MMC 1.0-1.2
	; 1 : SD V2  or MMC 1.4-2.2

	; Card flags set by card_init
	JMP_CARD_IO_FLAG_SET CARD_IO_FLG_IS_MMC, .is_mmc

	; Check the version of the CSD structure
	cmp al, 0
	je .is_sd_v1
	cmp al, 1

.is_sd_v2:
	printStringLn "      SD Version 2.0"

	; Extract C_SIZE from bit field
	xor cx, cx
	xor ax, ax
	mov al, [card_csd + 9]
	mov ah, [card_csd + 8]
	mov bl, [card_csd + 7]
	and bx, 0x3f
	mov [card_total_blocks], ax
	mov [card_total_blocks+2], bx

	jmp .size_computed

.is_mmc:
	printStringLn "      MMC card"
	jmp .decode_v1

.is_sd_v1:
	printStringLn "      SD Version 1.0"

.decode_v1:
	; Extract C_SIZE from bit field
	xor cx,cx
	xor ax,ax
	mov al, [card_csd + 7]
	shl ax, 1
	shl ax, 1
	or cx, ax
	xor ax, ax
	mov al, [card_csd + 8]
	rol al, 1
	rol al, 1
	and al, 3
	or cx, ax
	xor ax, ax
	mov ah, [card_csd + 6]
	and ah, 3
	shl ah, 1
	shl ah, 1
	or cx, ax
	mov [card_csd_csize], cx

	; Extract C_MULT from bit field
	xor cx, cx
	xor ax, ax
	mov al, [card_csd + 10]
	rol al, 1 ; Bit 7 -> bit 0
	and al, 1
	or cx, ax
	xor ax, ax
	mov al, [card_csd + 9]
	and al, 3
	shl al, 1
	or cx, ax
	mov [card_csd_cmult], cx

	; Extract write block size from bit field
	xor cx, cx
	xor ax, ax
	mov al, [card_csd + 12]
	and al, 3
	shl al, 1
	shl al, 1
	or cx, ax
	xor ax,ax
	mov al, [card_csd + 13]
	rol al, 1
	rol al, 1
	and al, 3
	or cx, ax
	mov [card_csd_block], cx

%if 0
	mov cx, [card_csd_csize]
	printString "         C_SIZE: "
	call printInt16

	mov cx, [card_csd_cmult]
	printString ", CSIZE_MULT: "
	call printInt16

	mov cx, [card_csd_block]
	printString ", WR_BL_LEN: "
	call printInt16
%endif

	; TODO : Compute card_total_blocks

	; BLOCKNR = (C_SIZE + 1) * MULT
	mov ax, [card_csd_csize]
	add ax, 1
	xor bx, bx

	; Mult
	; 0 : 4
	; 1 : 8
	; 2 : 16
	; ...
	; 7 : 512

	; So mult 0 is << 2
	shl ax, 1	; May generate a carry
	rcl bx, 1	; Carry goes in BX
	shl ax, 1	; May generate a carry
	rcl bx, 1	; Carry goes in BX

	mov cx, [card_csd_cmult]
	and cx, cx
	jz .shifted
.shift:
	shl ax, 1	; May generate a carry
	rcl bx, 1	; Carry goes in BX
	loop .shift
.shifted:

%if 0
	printString "       N blocks: "
	mov dx, bx
	call printHexWord
	mov dx, ax
	call printHexWord
	call newline
%endif

	; Now those blocks may be something other than 512 bytes..
	; WRITE_BL_LEN and READ_BL_LEN (always equal) encode the block
	; size like this:
	;
	; 9 : 512 bytes
	; 10 : 1024 bytes
	; 11 : 2048 bytes
	;
	; We are to store the 512 bytes block count in card_total_blocks,
	; so for 1024 bytes we multiply by 2, and for 2048 by 4.
	cmp word [card_csd_block], 10
	jl .doneBlkMult	; Min 9
	je .shiftBlk1 ; 10
	; Max 11
.shiftBlk2:
	shl ax, 1
	rcl bx, 1
.shiftBlk1:
	shl ax, 1
	rcl bx, 1
.doneBlkMult:
	mov [card_total_blocks], ax
	mov [card_total_blocks + 2], bx

.size_computed:


.display_size:
	printString "      Sectors: "

	printString "0x"
	mov dx, [card_total_blocks + 2]
	call printHexWord
	mov dx, [card_total_blocks]
	call printHexWord
	call newline


	printString "      SD-Cart geometry: "
	mov ax, ds
	mov es, ax
	mov si, card_total_blocks
	mov di, geometry
	call chs_fit
	call chs_display
	jmp .geo_ok
.geo_error:
	printStringLn "Error"
.geo_ok:

	clc ; No errror
	jmp .done


.cid_err:
	printStringLn "Error reading CID"
	stc
	jmp .done
.csd_err:
	printStringLn "Error reading CSD"
	stc
	jmp .done


.done:
	pop bp
	pop es
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; printMID: Print Card Manufacturer ID for value in AL
	;
	; Does not output a newline.
	;
printMID:
	cmp al, 0x01
	je .panasonic
	cmp al, 0x02
	je .toshiba
	cmp al, 0x03
	je .sandisk
	cmp al, 0x1b
	je .samsung
	cmp al, 0x27
	je .phison
	cmp al, 0x28
	je .lexar
	cmp al, 0x31
	je .silicon_power
	cmp al, 0x41
	je .kingston
	cmp al, 0x74
	je .transcend

	jmp .unknown

.panasonic:
	printString "Panasonic"
	jmp .done
.toshiba:
	printString "Toshiba"
	jmp .done
.sandisk:
	printString "SanDisk"
	jmp .done
.samsung:
	printString "Samsung"
	jmp .done
.phison:
	printString "Phison"
	jmp .done
.lexar:
	printString "Lexar"
	jmp .done
.silicon_power:
	printString "Silicon Power"
	jmp .done
.kingston:
	printString "Kingston"
	jmp .done
.transcend:
	printString "Transcend"
	jmp .done

.unknown:
	printString "unknown"
	stc
	ret
.done:
	clc
	ret



	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; getFirstOnlyFreePart : Point SI to the first partition if it is empty or the only one.
	;
	; Returns with CF set otherwise.
	;
getFirstFreeOnlyPart:
	push cx

	; Skip first
	mov si, mbrbuf + 0x1BE
	add si, 16	; Advance to next partition

	mov cx, 3
.next:
	cmp byte [si + 4], 0x00
	jne .non_empty_found
	add si, 16	; Advance to next partition
	loop .next

	mov si, mbrbuf + 0x1BE
	clc
	jmp .return

.non_empty_found:
	stc
	jmp .return

.return:
	pop cx
	ret


section .data

card_flags: db 0

default_mbr: incbin "mbr.bin"

bootsector:

section .bss

geometry: resb disk_geometry.size

card_csd: resb 16
; In 512 bytes blocks
card_total_blocks: resw 2
card_csd_csize: resw 1
card_csd_cmult: resw 1
card_csd_block: resw 1
card_cid: resb 16
part_type: resb 1
first_sector: resw 2
last_sector: resw 2
last_sect_cyl: resw 1
dos_major: resb 1
dos_minor: resb 1
tmpbuf: resb 512
mbrbuf: resb 512
