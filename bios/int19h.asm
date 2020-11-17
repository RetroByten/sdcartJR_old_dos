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
;;; File int19h.asm
;;;
;;; Code for int 19h service : Bootstrap loader
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bits 16
cpu 8086

section .text


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; install_int19: Replace the current int 19h handler by our own.
	;
install_int19:
	pushf
	cli
	push ds
	push ax
	push bx

	xor ax, ax	; Segment 0000
	mov ds, ax

	; Fetch current int19h
	mov ax, [19h * 4]		; Offset
	mov bx, [19h * 4 + 2]	; Segment

	; Move it
	mov [NEWINT19 * 4], ax
	mov [NEWINT19 * 4 + 2], bx

	; Install our own handler
	mov word [19h * 4], simple_int19
	mov [19h * 4 + 2], cs

	pop bx
	pop ax
	pop ds
	popf
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; load_bootsector: Load the first sector of a drive.
	;
	; Arg: DL = Drive number
	;
	; Loads it to 0000:7c00
	;
	; Returns with CF set if this fails.
	;
load_bootsector:
	push ax
	push bx
	push cx
	push dx
	push es

	mov ax, 0		; ES:BX Target location
	mov es, ax
	mov bx, 0x7c00
	mov ah, 0x02	; Read Disk Sectors
	mov al, 1		; Length: 1 Sector
	mov cx, 1		; Track 0, sector 1 (first)
	mov dh, 0		; Head 0
	;	mov dl, 0x80
	int 13h

	pop es
	pop dx
	pop cx
	pop bx
	pop ax

	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; check_marker: Look at 0000:7DFE for AA55 marker.
	;
	; Returns with ZF set if found.
	;
check_marker:
	push ax
	push es

	xor ax, ax
	mov es, ax

	; Check if 55aa marker is present
	mov ax, [es:0x7dfe]
	cmp ax, 0xaa55

	pop es
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; reset_disk_system: Call int13h/AH=00
	;
	; Arg: DL = Drive number
	;
	; Returns with CF set on error
	;
reset_disk_system:
	push ax
	mov ax, 0x0000
	int 13h
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; simple_int19: Simple int 19h implementation.
	;
	; Try booting from floppy first, then try booting from a card.
	; If all fails, invoke int 18h to enter BASIC.
	;
simple_int19:
	push ax
	push bx
	push cx
	push dx
	push ds
	push es

	; restore data segment
	mov ax, cs
	mov ds, ax

	;;; 1. Try booting from floppy
	mov cx, FLOPPY_BOOT_ATTEMPTS
	cmp cx, 0
	je .floppy_no_boot
.floppy_retry:
	mov dl, BOOT_FLOPPY_DRIVE_NUMBER
	call load_bootsector    ; Try loading boot sector from floppy
	jc .floppy_load_failed	; Did it work?
	call check_marker		; Yes! Check for a boot signature
	jne .floppy_no_boot		; None? No point retrying, go on to boot from card
	jmp 0x0000:0x7c00		; Found? Then jump at it!
.floppy_load_failed:		; Loading failed. Retry...
	call reset_disk_system
	loop .floppy_retry		; Keep trying?
	; Ok, all attempts failed. Probably no floppy present.
.floppy_no_boot:


	;;; 2. Try booting from the card

	; Fetch the boot sector
	mov dl, 0x80
	call load_bootsector
	jc .failed

	; Check if 55aa marker is present
	call check_marker
	jne .nomarker

	printStringLn "Booting from SD-Cart JR..."
.boot:
	jmp 0x0000:0x7c00		; Found? Then jump at it!

.nomarker:
	printStringLn "SD-Cart JR: Invalid boot sector"
	jmp .giveup

.failed:
	printStringLn "SD-Cart JR: Could not read boot sector"

	; Call the original int 18h which was restored above.
.giveup:
	printStringLn "Entering BASIC..."
	call delay	; Wait one moment to let the user see the error message

	pop es
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax

	pop ax	; IP
	pop ax	; CS
	popf	; Flags
	int 18h
	iret


