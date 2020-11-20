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

section .text



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
	;
	;
card_init:
	push ax
	push bx
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

	clc

.done:
	pop ds
	pop es
	pop bp
	pop dx
	pop cx
	pop bx
	pop ax
	ret


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
	call blockToByteAddress
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
	call blockToByteAddress
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
	call blockToByteAddress
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
	call blockToByteAddress
.read_next_sector:
	mov dx, ax  ; Save AX (will contain cmd17 result code)
	call card_cmd17
	jc .error
	mov ax, dx ; Restore AX
	; Advance to next block
	add bx, 512
	adc ax, 0
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
	;        CX is sector count
	;		 ES:SI Source buffer
	;
	; Returns with carry set on timeout.
	;
	; TODO : Support block-adressed cards
	; TODO : Try using the multiple-write block command (CMD25)
	;
card_writeSectors:
	push ax
	push bx
	push cx
	push dx
	push si

	; Convert to a byte address
	call blockToByteAddress

.write_next_sector:
	call card_cmd24
	jc .error
	cmp dl, 0x05
	jne .error
	; Advance to next block
	add bx, 512
	adc ax, 0
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

