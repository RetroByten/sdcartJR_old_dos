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
;;; File card_io.asm
;;;
;;; High level (relatively) functions for card init, info, read and write.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bits 16
cpu 8086

%define INIT_RETRIES	10


%define CARD_IO_FLG_IS_MMC			0x01
%define CARD_IO_FLG_BLOCK_ADRESSING	0x02
%define CARD_IO_FLG_IS_SDV2			0x04

section .text


card_pulseLed:
	call spi_select
	call spi_deselect
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_powerup : Perform the power up sequence
	;
	; Device may use up to 74 clocks for preparation before
	; receiving the first command.
	;
	; Send 100 cycles to be on the safe side.
	;
card_powerup:
	call spi_deselect
	push cx
	mov cx, 100
	call spi_justclock
	pop cx
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_init : Intialisation sequence.
	;
	; Returns card size in blocks in AX:BX (BX is MSW)
	;
	; Returns with carry set if init fails. In that case, AX:BX is undefined.
	;
card_init:
	push cx
	push dx
	push bp
	push es
	push ds

	CLR_CARD_IO_FLAGS

	call card_powerup

%ifdef TRACE_CARD_INIT
	printStringLn "card_init start"
%endif

	mov cx, INIT_RETRIES
.retry_cmd0:
	call card_cmd0
	cmp al, 0x01
	je .cmd0_ok
	cmp al, 0x00
	je .cmd0_ok ; TODO: Skip cmd8 and acmd41? (0 means non-idle)
%ifdef TRACE_CARD_INIT
	printString "!"
%endif
	call card_init_retry_delay
	loop .retry_cmd0
	jmp .init_failed

.cmd0_ok:

	mov cx, INIT_RETRIES
.long_process:

	; Try CMD8 first.
	mov bp, tmpbuf
	call card_cmd8
	and al, 0xFE ; Accept 0 and 1
	jz .cmd8_ok

	; CMD8 may not be supported. Try ACMD41 in that case.
	jmp .try_acmd41

.cmd8_ok:
%ifdef TRACE_CARD_INIT
	printString "CMD8 OK: "
	mov cx, 6
	call hexdump
	call newline
%endif

.try_acmd41:

	; Send ACMD41
.retry_acmd41:
	call card_acmd41
	jc .init_failed
	cmp al, 0x01
	je .retry
	cmp al, 0x00
	jz .acmd41_ok

	jmp .try_cmd1

.retry:
%ifdef TRACE_CARD_INIT
	printString "."
%endif
	call card_init_retry_delay
	loop .retry_acmd41
	jmp .init_failed

.cmd1_busy:
%ifdef TRACE_CARD_INIT
	printStringLn "CMD1 retry"
%endif
	and cx, cx
	dec cx
	jz .init_failed
.try_cmd1:
	call card_cmd1
	cmp al, 0x01
	je .cmd1_busy
	cmp al, 0x00
	je .cmd1_ok ; TODO: Skip cmd8 and acmd41? (0 means non-idle)

%ifdef TRACE_CARD_INIT
	printStringLn "CMD1 failed"
%endif
	jmp .init_failed

.acmd41_ok:

%ifdef TRACE_CARD_INIT
	push dx
	printString "ACMD41 OK, R1="
	mov dl, al
	call printHexByte
	call newline
	pop dx
%endif


.check_adressing:
	push ax
	push bx
	call card_cmd58
	test ax, 0x4000	; Check for CCS bit
	pop bx
	pop ax

	; When bit 30 is set, it is a high capacity card
	jz .byte_adressed

.block_adressed:
	SET_CARD_IO_FLAG	CARD_IO_FLG_BLOCK_ADRESSING
	jmp .init_ok
.byte_adressed:
	jmp .init_ok


.cmd1_ok:

	SET_CARD_IO_FLAG	CARD_IO_FLG_IS_MMC

%ifdef TRACE_CARD_INIT
	printStringLn "CMD1 OK"
%endif
	jmp .init_ok

.init_failed:
	stc
	jmp .done

.init_ok:

	; Make sure CRCs are disabled. It should not be necessary.
	call card_cmd59

	; Use 512 bytes blocks.
	call card_cmd16


	;;; Read the CSD reg and compute the card size from it.
	; Allocate a buffer for the contents of the CSD reg.
	;
	push di
	push si
		; borrow stack
		sub sp, 16
		mov di, sp

		mov ax, ss
		mov es, ax
		; read the CSD regiser to ES:DI
		call card_readCSD

		mov si, di	; card_getSizes wants it in ES:SI
		call card_getSize
		; card_getSize also sets some flags regarding
		; the card type, and eventually, adressing type.

		; AX:BX contains card size, returned by this subroutine

		add sp, 16 ; free stack

	pop si
	pop di

	clc

.done:
	pop ds
	pop es
	pop bp
	pop dx
	pop cx
	ret



%macro INC_AXBX_PER_ADRESSING 0
JMP_CARD_IO_FLAG_SET CARD_IO_FLG_BLOCK_ADRESSING, %%byOne
add bx, 512
adc ax, 0
jmp %%done
%%byOne:
add bx, 1
adc ax, 0
%%done:
%endmacro



	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_readSectors :
	;
	; Input: dword in AX..BX - same order as geo2block
	;                         AH AL BH BL
	;                         31........0
	;        CX is sector count
	;		 DS:BP : Destination buffer
	;
	; Returns with carry set on timeout.
	;
	; TODO : Support block-adressed cards
	;
card_readSectors:
	cmp cx, 1
	je .readSingleSector

;	jmp .no_multiple_read

	JMP_CARD_IO_FLAG_SET CARD_IO_FLG_IS_MMC, .multi_block_read_mmc

	;;; Reads multiple blocks using CMD18 (SD card version)
	;
.multi_block_read_sd:
	; Go right ahead and issue CMD18. The card starts sending
	; blocks...
	push ax
	push bx
	call _convertAddress
	call card_cmd18
	jc .read_multi_sd_failed
	; The operating must be stopped with CMD12.
	call card_cmd12
.read_multi_sd_failed:
	pop bx
	pop ax
	ret

	;;; Reads multiple blocks using CMD18 (MMC card version)
	;
.multi_block_read_mmc:
	; First we must tell the card how many blocks we will read.
	push ax ; Preserve the block number
	call card_cmd23	; Set block count (arg is CX)
	pop ax
	jc .set_block_count_failed
	; Now read the blocks.
	push ax
	push bx
	call _convertAddress
	call card_cmd18
.set_block_count_failed:
	pop bx
	pop ax
	ret

	;;; Read a single sector ;;;
	;
.readSingleSector:
	push ax
	push bx
	push dx
	call _convertAddress
	call card_cmd17
	jc .error_reading_one_sector
	clc
.error_reading_one_sector:
	pop dx
	pop bx
	pop ax
	ret

	;;; Original implementation (repeated calls to cmd17)	;;;
	;
.no_multiple_read:
	push ax
	push bx
	push cx
	push dx
	push bp

	; Expand the sector address to a byte address
	call _convertAddress
.read_next_sector:
	mov dx, ax  ; Save AX (will contain cmd17 result code)
	call card_cmd17
	jc .error
	mov ax, dx ; Restore AX
	; Advance to next block
	INC_AXBX_PER_ADRESSING
	; Advance buffer position
	add bp, 512
	loop .read_next_sector

	clc
.error:

	pop bp
	pop dx
	pop cx
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_writeSectors :
	;
	; Input: dword in AX..BX - same order as geo2block
	;                         AH AL BH BL
	;                         31........0
	;        CX is sector count
	;		 ES:SI Source buffer
	;
	; Returns with carry set on timeout.
	;
	; TODO : Support block-adressed cards
	;
card_writeSectors:
	cmp cx, 1
	je .write_single_sector

	JMP_CARD_IO_FLAG_SET CARD_IO_FLG_IS_MMC, .multi_block_write_mmc

	;;; Write multiple block using CMD25, for SD cards
	;
.multi_block_write_sd:
	push ax
	push bx
	push dx
	call _convertAddress
	call card_cmd25
	pop dx
	pop bx
	pop ax
	ret

	;;; Write multiple blocks for MMC (use a set block count command)
.multi_block_write_mmc:
	push ax
	push bx
	push dx

%if 0
	; Fails on my test MMC card.. But appears to work anyway...
	; Maybe it depends on the spec revision the card complies to?
	push ax ; Preserve the block number
	call card_cmd23	; Set block count (arg is CX)
	mov dl, al ; Copy error code
	pop ax
	jc .set_block_count_failed
%endif
	call _convertAddress
	call card_cmd25
	jmp .multi_block_mmc_done
.set_block_count_failed:
.multi_block_mmc_done:
	pop dx
	pop bx
	pop ax
	ret


	;;; Original implementation with repeated card_cmd24 calls
	;
.no_multiple_write:
	push ax
	push bx
	push cx
	push dx
	push si

	call _convertAddress

.write_next_sector:
	call card_cmd24
	jc .error
	cmp dl, 0x05
	jne .error
	; Advance to next block
	INC_AXBX_PER_ADRESSING
	; Advance buffer position
	add si, 512
	loop .write_next_sector

	clc
	jmp .ret

.error:
	stc

.ret:
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret

	;;; Write a single sector ;;;
	;
.write_single_sector:
	push ax
	push bx
	push dx
	call _convertAddress
	call card_cmd24
	pop dx
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_readCID : Read the card CID register
	;
	; ES:DI : Destination buffer
	;
	; Returns with carry set on timeout.
	;
card_readCID:
	push ax
	; R1 value returned in AL
	call card_cmd10
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_readCSD : Read the card CSD register
	;
	; ES:DI : Destination buffer
	;
	; Returns with carry set on timeout.
	;
card_readCSD:
	push ax
	; R1 value returned in AL
	call card_cmd9
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_getSize: Analyze CSD to determine the card size, in 512 byte blocks
	;
	; Arguments:  ES:SI : Pointer to a buffer containing the CSD register
	;
	; Returns card size in AX:BX (AX LSW, BX MSW)
	;
	; Normally called by card_init
	;
card_getSize:
	push cx
	push dx
	push di
	push bp

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
	mov al, [es:si + 0]
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
	SET_CARD_IO_FLAG CARD_IO_FLG_IS_SDV2

	; Extract C_SIZE from bit field
	mov al, [es:si + 9]
	mov ah, [es:si + 8]
	mov bl, [es:si + 7]
	and bx, 0x3f

	; Memory capacity = (C_SIZE+1) * 512 * 1024

	add ax, 1	; Do C_SIZE + 1
	adc bx, 0

	; We want a 512-byte block count, therefore the * 512 can be dropped;
	; One thing left to do: Multiply by 1024

	; SHL 8		(*256)
	mov bh, bl
	mov bl, ah
	mov ah, al
	xor al, al
	; SHL 1 (*512)
	shl ax, 1	; Shift AX to the left, bit 7 into carry
	rcl bx, 1	; Shift BX to the left, bit 0 from carry
	; SHL 1 (*1024)
	shl ax, 1	; Shift AX to the left, bit 7 into carry
	rcl bx, 1	; Shift BX to the left, bit 0 from carry

	; Size is already in AX:BX, ready for return.
	jmp .size_computed

.is_mmc:
	jmp .decode_v1

.is_sd_v1:
	CLR_CARD_IO_FLAG CARD_IO_FLG_IS_SDV2

.decode_v1:
	; Extract C_SIZE from bit field
	xor cx,cx
	xor ax,ax
	mov al, [es:si + 7]
	shl ax, 1
	shl ax, 1
	or cx, ax
	xor ax, ax
	mov al, [es:si + 8]
	rol al, 1
	rol al, 1
	and al, 3
	or cx, ax
	xor ax, ax
	mov ah, [es:si + 6]
	and ah, 3
	shl ah, 1
	shl ah, 1
	or cx, ax
	;mov [card_csd_csize], cx
	mov dx, cx ; DX = csd_csize

	; Extract C_MULT from bit field
	xor cx, cx
	xor ax, ax
	mov al, [es:si + 10]
	rol al, 1 ; Bit 7 -> bit 0
	and al, 1
	or cx, ax
	xor ax, ax
	mov al, [es:si + 9]
	and al, 3
	shl al, 1
	or cx, ax
	;mov [card_csd_cmult], cx
	mov di, cx ; DI = csd_cmult

	; Extract write block size from bit field
	xor cx, cx
	xor ax, ax
	mov al, [es:si + 12]
	and al, 3
	shl al, 1
	shl al, 1
	or cx, ax
	xor ax,ax
	mov al, [es:si + 13]
	rol al, 1
	rol al, 1
	and al, 3
	or cx, ax
	;mov [card_csd_block], cx
	mov bp, cx ; BP = csd_block

%if 0
	push cx
	;mov cx, [card_csd_csize]
	mov cx, dx ; DX holds csd_csize
	printString "         C_SIZE: "
	call printInt16

	mov cx, di ; DI holds csd_cmult
	printString ", CSIZE_MULT: "
	call printInt16

	mov cx, bp ; BP holds csd_block
	printString ", WR_BL_LEN: "
	call printInt16
	pop cx
%endif

	; BLOCKNR = (C_SIZE + 1) * MULT
	mov ax, dx ; DX holds csd_csize
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

	mov cx, di ; // DI holds csd_cmult
	and cx, cx
	jz .shifted
.shift:
	shl ax, 1	; May generate a carry
	rcl bx, 1	; Carry goes in BX
	loop .shift
.shifted:

%if 0
	push dx
	printString "       N blocks: "
	mov dx, bx
	call printHexWord
	mov dx, ax
	call printHexWord
	call newline
	pop dx
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
	;cmp word [card_csd_block], 10
	cmp bp, 10 ; BP holds csd_block
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

	; Size is already in AX:BX

.size_computed:

	pop bp
	pop di
	pop dx
	pop cx
	ret

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; _convertAddress : Get byte address from a block, it necessary.
	;
	; Depending on the CARD_IO_FLG_BLOCK_ADRESSING flag.
	;
	; Input: dword in AX..BX - same order as geo2block
	;
	; Output: Byte address in AX..BX.
	;
_convertAddress:
	; Do nothing for block-addressed cards
	JMP_CARD_IO_FLAG_SET CARD_IO_FLG_BLOCK_ADRESSING, .done

	; Convert to byte address
	; << 8
	mov ah, al
	mov al, bh
	mov bh, bl
	xor bl, bl
	; << 1
	shl bx, 1 ; Bit 7 goes to carry
	rcl ax, 1 ; carry goes to bit 0
.done:
	ret



