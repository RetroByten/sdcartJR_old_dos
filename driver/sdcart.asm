; SD-Cart JR : PCJR Card reader cartridge
; Copyright (C) 2020-2021 Raphael Assenat <raph@raphnet.net>
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
;;; File sdcart.asm
;;;
;;; DOS Block Device Driver for the SD-Cart JR cartridge.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

[map all sdcart.map]
org 0x00
cpu 8086
bits 16


%ifdef LOWMEM
; The unrolled 512-byte spi receive function is faster (~40 kB/s instead of 33 kB/s)
; but uses an additional 4.5K of memory.
%define NO_UNROLL_READ512
; Roughtly the same tradeoff for writes.
%define NO_UNROLL_WRITE512
%define BANNER_STR "sdcartl.sys v0.5"
%else
%define BANNER_STR "sdcart.sys v0.5"
%endif

%undef TRACE_ERRORS
%undef TRACE_CARD_INIT
%undef TRACE_READS
%undef TRACE_COMMANDS
%undef TRACE_UNIMPLEMENTED_COMMANDS
%define INIT_RETRIES 255

%define ASK_INSTALL

; Include STRUC defs for driver data structures
%include 'drv.inc'

%macro MESSAGE 1
%ifndef LOWMEM
push ax
push ds
mov ax, cs
mov ds, ax
printStringLn %1
pop ds
pop ax
%endif
%endmacro

section .text

;;; Device Header
;
; This must come first, do not add/include code before this!
;
device_header:
	dd -1     ; Pointer to next device
	; Attributes (bit 15 = 0 for Block device)
	;             bit 1  = 1 to indicate support for >32M partitions)
	dw 0x0002
	dw _strategy
	dw _interrupt
	db 1
	times 7 db 0


; Stores request header address
reqseg: dw 0
reqoff: dw 0

; Stores partition info here
part_type: db 0
part_off: dw 0,0

; Used to report that media was changed on
; the first call.
force_init: db 1

; card_io.asm requires some macros to store information
; about the card type and features. How those are stored
; is application specific. In the case of this device driver,
; cs:flags is used.
flags: db 0
%macro JMP_CARD_IO_FLAG_SET 2
test byte [cs:flags], %1
jnz %2
%endmacro
%macro SET_CARD_IO_FLAG 1
or byte [cs:flags], %1
%endmacro
%macro CLR_CARD_IO_FLAG 1
and byte [cs:flags], ~(%1)
%endmacro
%macro CLR_CARD_IO_FLAGS 0
mov byte [cs:flags], 0
%endmacro

%include 'strutil.asm'
%include 'spi.asm'
%include 'card_cmd.asm'
%include 'card_io.asm'
%include 'mbr.asm'
;%include 'hexdump.asm'
%define NO_GEO2BLOCK
%include 'chs2lba.asm'

section .text

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; _strategy: Device driver "strategy" routine. Far-called by DOS.
	;
	; Called with pointer in ES:BX
	;
	; Just save the pointer somewhere. The work will be done in _interrupt
	;
_strategy:
	push ax
	mov ax, es
	mov [cs:reqseg], es
	mov [cs:reqoff], bx
	pop ax
	retf


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; _interrupt: Device driver "interrupt" routine. Far-called by DOS.
	;
_interrupt:
	push ax
	push bx
	push cx
	push dx
	push ds
	push es
	push si
	push di
	push bp
	pushf

	cld

	; Point ES:BX to request
	mov ax, [cs:reqseg]
	mov es, ax
	mov bx, [cs:reqoff]


	mov al, [es:bx + request_header.command]

%ifdef TRACE_COMMANDS
	push ax
	push dx
	push ds
	printString "["
	mov dl, al
	call printHexByte
	printString "]"
	pop ds
	pop cx
	pop ax
%endif

	cmp al, CMD_INIT
	je command_init
	cmp al, CMD_MEDIA_CHECK
	je command_media_check
	cmp al, CMD_BUILD_BPB
	je command_build_bpb
	cmp al, CMD_INPUT
	je command_read
	cmp al, CMD_OUTPUT
	je command_write

.unimplemented:

%ifdef TRACE_UNIMPLEMENTED_COMMANDS
	printString "unimpl. cmd: "
	push dx
	mov dl, [es:bx + request_header.command]
	call printHexByte
	pop dx
	call newline
%endif

	mov word [es:bx + request_header.status], 0x8003	; error - unknown command
	jmp _interrupt.cleanup

.cleanup:
	popf
	pop bp
	pop di
	pop si
	pop es
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	retf


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; command_init: Subroutine for device driver command 0: INIT
	;
command_init:
	printStringLn BANNER_STR
%ifndef LOWMEM
	printStringLn "By Raphael Assenat"
%endif

%ifdef ASK_INSTALL
	printString "Install (y/N)?"
	mov ah, 0x01
	int 21h
	cmp al, 'y'
	je .install
	cmp al, 'Y'
	je .install

.skip:
	call newline
%ifndef LOWMEM
	printStringLn "Not installing"
%endif
	mov byte [es:bx + init_request.num_units], 0

	; Free all memory used by this driver
	mov word [es:bx + init_request.break_address], device_header
	mov ax, cs
	mov word [es:bx + init_request.break_address + 2], ax

	jmp _interrupt.cleanup
%endif

.install:
	call newline

	call card_powerup


	mov byte [es:bx + init_request.num_units], 1

	mov word [es:bx + init_request.break_address], _memend + 1
	mov ax, cs
	mov word [es:bx + init_request.break_address + 2], ax

	mov word [es:bx + init_request.bpb_ptr], bpb_array
	mov word [es:bx + init_request.bpb_ptr + 2], ax
%ifndef LOWMEM
	printStringLn "SD-Cart JR Installed"
%endif
	jmp _interrupt.cleanup


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; read_cid: Read the CID register
	;
	; Writes 16 bytes to [cidbuf]
	;
	; CF set on error (timeout or card not ready)
	;
read_cid:
	push di
	push es

	push cs
	pop es
	mov di, cidbuf
	call card_readCID
	jc .error

	jmp .ok

.error:
	stc
	jmp .ret

.ok:
	clc
.ret:
	pop es
	pop di
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; partition_scan: Read MBR and look for a usable partition
	;
	; Sets [part_type] and [part_off] if found, returning with CF clear.
	;
	; CF set if no usable partition was found.
	;
partition_scan:
	push ax
	push bx
	push dx
	push ds
	push si

	; Setup the data segment
	mov ax, cs
	mov ds, ax
	; Prepare buffer
	mov bp, tmpbuf
	; Read MBR
	call mbr_read
	jc .no_partitions
	; Prepare buffer
	mov si, tmpbuf
	; Scan for a suitable partition
	call mbr_getPartitionOffset
	jc .no_partitions
	mov [part_type], dl
	mov [part_off], bx
	mov [part_off+2], ax

%ifndef LOWMEM
	; TODO : Add a command-line argument to disable this
	printString "Found partition type "
	call printHexByte
	printString " at "
	mov dx, ax
	call printHexWord
	mov dx, bx
	call printHexWord
	call newline
%endif

	clc
	jmp .part_scan_done
.no_partitions:
%ifndef LOWMEM
	printString "No suitable partition found."
%endif
	stc
.part_scan_done:

	pop si
	pop ds
	pop dx
	pop bx
	pop ax

	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; command_media_check: Subroutine for device driver command 1: MEDIA CHECK
	;
command_media_check:

	; If the computer was rebooted, the card may be already
	; initialized and CMD13 won't fail or return idle. So only the first
	; time, force a card init.
	test byte [cs:force_init], 1
	jnz .card_init

	; Start by reading the card status (CMD13). If the card is idle, or if the
	; command fails, it means the card was changed or removed. A card that was
	; previously initialized will not be idle.
	call card_cmd13
	jc .card_changed
	cmp al, 0x01	; Idle? This means the card was changed.
	je .card_changed

;	call read_cid
;	jc .card_changed

	; If it did not fail, assume the card was not changed. (Should be true unless
	; another program initialized the card. To detect this, the actual card ID should
	; be kept and compared...
.card_ready:
	mov word [es:bx + request_header.status], RQ_STATUS_BIT_DONE
	;mov byte [es:bx + media_check.return], 0 ; Cannot know if card was changed
	mov byte [es:bx + media_check.return], MEDIA_CHECK_RET_NOT_CHANGED ; no change
	jmp _interrupt.cleanup

.card_changed:
	MESSAGE "Card changed"
.card_init:

	; card_init returns the card size in AX:BX. Not used, but we need to preserve
	; the original BX value..
	push bx
	call card_init
	pop bx

	jc .failed
	JMP_CARD_IO_FLAG_SET CARD_IO_FLG_IS_MMC,.is_mmc
.not_mmc:
	MESSAGE "SD card inserted"
	jmp .scan_partitions
.is_mmc:
	MESSAGE "MMC card inserted"
.scan_partitions:
	call partition_scan
	jc .failed
	call read_cid
	jc .failed

	; Clear the "first time" bit
	and byte [cs:force_init], ~1

	mov word [es:bx + request_header.status], RQ_STATUS_BIT_DONE
	;mov byte [es:bx + media_check.return], 0 ; Cannot know if card was changed
	mov byte [es:bx + media_check.return], MEDIA_CHECK_RET_CHANGED
	jmp _interrupt.cleanup

.failed:
	MESSAGE "Init failed"
	mov word [es:bx + request_header.status], RQ_STATUS_BIT_DONE | RQ_STATUS_BIT_ERR | RQ_STATUS_ERR_DRIVE_NOT_READY
	mov byte [es:bx + media_check.return], 0 ; Cannot know if card was changed

	jmp _interrupt.cleanup


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; command_build_bpb: Subroutine for device driver command 2: BUILD BPB
	;
command_build_bpb:
	; Partition first sector contains the BPB. Read the sector, then return a pointer
	; to the BPB.
	push bx
		mov ax, cs ; Establish data segment
		mov ds, ax
		mov bx, [cs:part_off]		; Read low word
		mov ax, [cs:part_off + 2]	; Read high word
		mov bp, tmpbuf	; Sector buffer
		mov cx, 1
		call card_readSectors
	pop bx
	jc .fail

.ok:
	; The BPB is at offset 0xB in sector buffer.
	mov word [es:bx + buildbpb.bpb], tmpbuf + 0xB
	mov word [es:bx + buildbpb.bpb + 2], cs
	mov word [es:bx + request_header.status], RQ_STATUS_BIT_DONE
	jmp _interrupt.cleanup

.fail:
	mov word [es:bx + request_header.status], RQ_STATUS_BIT_DONE | RQ_STATUS_BIT_ERR | RQ_STATUS_ERR_DRIVE_NOT_READY
	jmp _interrupt.cleanup


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; command_read: Subroutine for device driver command 4: INPUT (read)
	;
command_read:
	push bx ; Preserve BX
		mov cx, [es:bx + rwcommand.count]	; Sector count

		mov bp, [es:bx + rwcommand.addr]	; Destination address
		mov ax, [es:bx + rwcommand.addr + 2]
		mov ds, ax

		mov ax, [es:bx + rwcommand.start]	; Start sector
		cmp ax, 0xFFFF
		je .use_lBigSect
		mov bx, ax
		xor ax, ax
		jmp .startSectorKnown
.use_lBigSect:
		mov ax, [es:bx + rwcommand.lBigSect]	; Start sector (LSW)
		mov bx, [es:bx + rwcommand.lBigSect + 2]	; Start sector (MSW)
		xchg ax, bx ; AX is most significant
.startSectorKnown:

		; Add the partition offset
		add bx, [cs:part_off]
		adc ax, [cs:part_off + 2]

%ifdef TRACE_READS
		; rXXXXXXXX:XXXX.
		push dx
		mov dl, 'r'
		call putchar
		mov dx, ax
		call printHexWord
		mov dx, bx
		call printHexWord
		mov dl, ':'
		call putchar
		mov dx, cx
		call printHexWord
		mov dl, '.'
		call putchar
		pop dx
%endif

		; AX..BX is the sector number, CX is count, DS:BP destination
		call card_readSectors
	pop bx

	; If card_readSectors returned an error, carry is still set
	jc .fail

	mov word [es:bx + request_header.status], RQ_STATUS_BIT_DONE
	jmp _interrupt.cleanup

.fail:
	mov word [es:bx + request_header.status], RQ_STATUS_BIT_ERR | RQ_STATUS_BIT_DONE | RQ_STATUS_ERR_DRIVE_NOT_READY
	mov word [es:bx + rwcommand.count], 0 ; No sectors read
	jmp _interrupt.cleanup


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; command_write : Subroutine for device driver command 8: OUTPUT (write)
	;
command_write:
	; ES:BX points to the request structure. Save it, we need it in the end to
	; return the result.
	push bx
	push es

		mov cx, [es:bx + rwcommand.count]	; Sector count

		; Prepare ES:SI (source buffer)
		mov si, [es:bx + rwcommand.addr]	; Destination address
		mov dx, [es:bx + rwcommand.addr + 2];
		; cannot set ES just yet (needed below)

		; Prepare address in AX/BX (BX low word, AX high word)
		cmp word [es:bx + rwcommand.start], 0xffff
		je .use_lBigSect
		mov bx, [es:bx + rwcommand.start]	; Start sector
		xor ax, ax ; Command only has 16-bit sector number..
		jmp .startSectorKnown
.use_lBigSect:
		mov ax, [es:bx + rwcommand.lBigSect]	; Start sector (LSW)
		mov bx, [es:bx + rwcommand.lBigSect + 2]	; Start sector (MSW)
		xchg ax, bx ; AX is most significant
.startSectorKnown:

		; ES not used past this point, set it now.
		mov es, dx

		; Add the partition offset
		add bx, [cs:part_off]
		adc ax, [cs:part_off + 2]

		call card_writeSectors

	; Restore ES:BX (request)
	pop es
	pop bx

	; If card_writeSectors failed, carry is still set here.
	jc .failed

.ok:
	mov word [es:bx + request_header.status], RQ_STATUS_BIT_DONE
	; No need to touch the sector count as we wrote exactly what
	; was requested!
	jmp _interrupt.cleanup

.failed:
	mov word [es:bx + request_header.status], RQ_STATUS_BIT_ERR | RQ_STATUS_BIT_DONE | RQ_STATUS_ERR_WRITE_FAULT
	mov word [es:bx + rwcommand.count], 0
	jmp _interrupt.cleanup


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




section .data

bpb_array:
	times 4 dd bpb

bpb:
	db 0x00, 0x02, 0x04, 0x01, 0x00, 0x02, 0x00, 0x02, 0xb1, 0xff, 0xf8, 0x40, 0x00, 0x3f, 0x00, 0x10, 0x00, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
	times 80 db 0

section .bss

cidbuf: resb 16

tmpbuf: resb 512


ALIGN 16, db 0
_memend: resb 1
