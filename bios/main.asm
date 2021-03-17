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
;;; File main.asm
;;;
;;; BIOS Main / Top level file
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

org 0x0000
bits 16
cpu 8086
[map symbols main.map]


%define NEWINT13	68h		; This is where the original int13h will be moved
%define NEWINT19	6Ah		; This is where the original int19h will be moved

; The bootstrap loader (int 19h) will try booting from this floppy first.
%define BOOT_FLOPPY_DRIVE_NUMBER	0

; Number of attempts loading a boot sector from floppy until giving up and
; trying to boot from the SD card. If zero, floppy boot is skipped.
%define FLOPPY_BOOT_ATTEMPTS	1


section .text

	; This must come first, before any other code or includes.
entry:
	db 0x55, 0xAA   ; Marker
	db 0            ; Size / 512
	jmp init        ; Code follows
	db 0

; This project does not use an external linker. Code from other files is
; simply included.
%include 'strutil_bios.asm'
%include 'print16.asm'
%include 'spi.asm'

; This provides some macros used card_io to store flags
%include 'memory.asm'

%undef TRACE_ERRORS
%include 'card_io.asm'

;%define TRACE_CARD_INIT
%include 'card_cmd.asm'


%include 'int13h.asm'
%include 'int19h.asm'

%include 'kb.asm'
%include 'menu.asm'

;%include 'hexdump.asm'

section .text


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; init: Install the BIOS (replace interrupt handlers) and try to detect an SD Card.
	;
	; No arguments
	; Nothing returned, no trashed registers.
	;
	; This is called from the PCjr BIOS boot as part of the extension ROM/Cartridge
	; scan. The current strategy is to install our own int 13h and int 18h handlers, then
	; return.
	;
	; A bit later, the PCjr BIOS will try booting from floppy. It this fails, it will
	; call int 19h which would normally enter BASIC. Except in this case, the new int 19h
	; code will run and try booting from the SDCard. (If this fails, BASIC will be entered)
	;
init:
	push ax
	push bx
	push cx
	push dx
	push ds

	; Setup the data segment
	mov ax, cs
	mov ds, ax

	; Set a video mode here to clear the IBM logo. Otherwise the output is
	; a mix of colored and invisible characters.
	mov ah, 0
	mov al, 3
	int 10h

	; Begin by showin the banner
	printString "SD-Cart JR v1 BIOS v"
	printStringLn VERSION_STR
	printStringLn "(C) Copyright 2020-2021 Raphael Assenat"
	call newline

	call memory_clearStoredData

	; Now check if there already is a hard drive.
	call get_nfdsks
	cmp ax, 0
	ja .multidrive_setup

	; When the SD-Cart JR is the only "Hard drive"
.singledrive_setup:
	call offer_options_menu_single
	cmp bl, MENU_OPTION_SINGLE_DRIVE
	je .int_setup_hd0
	; The only alternative to installing the BIOS is not installing it.
	jmp .sdcart_jr_disabled

	; When there is more than one drive, the SD-Cart JR can either:
	;
	; - Become first drive, displacing the drives already present by
	;   translating the DL value in subsequant int13h calls.
	;
	; - Install as the last drive, incrementing by one the value returned
	;   in DL when the original int 13h handler is invoked.
.multidrive_setup:
	call offer_options_menu_multidrive
	cmp bl, MENU_OPTION_FIRST_DRIVE
	je .int_setup_hd0_translate
	cmp bl, MENU_OPTION_SINGLE_DRIVE
	je .int_setup_hd0
	cmp bl, MENU_OPTION_NO_INSTALL
	je .sdcart_jr_disabled

	; Default option: MENU_OPTION_LAST_DRIVE

.install_as_last_drive:
	cmp ax, 1
	je .int_setup_hd1
	cmp ax, 2
	je .int_setup_hd2
	cmp ax, 3
	je .int_setup_hd3

.too_many_hard_drives:
	printStringLn "Error: Too many drives ( > 4 )."
.sdcart_jr_disabled:
	printStringLn "SD-Cart JR disabled."
	jmp .init_done

	; Install a new int 13h handler for disk 0x80 (hd0)
	; Also installs a new 19h hanler for booting from the SD-Card JR, since it is
	; the first drive.
.int_setup_hd0:
	mov dx, int13h_card_drive80
	call install_int13h
	printStringLn "Int 13h installed for drive 80h"
	call install_int19
	printStringLn "Int 19h installed"
	mov al, 0x80
	jmp .int_setup_done

	; Install a new int 13h handler for disk 0x81 (hd1)
.int_setup_hd1:
	mov dx, int13h_card_drive81_fixcount
	call install_int13h
	printStringLn "Int 13h installed for drive 81h"
	mov al, 0x81
	jmp .int_setup_done

	; Install a new int 13h handler for disk 0x82 (hd2)
.int_setup_hd2:
	mov dx, int13h_card_drive82_fixcount
	call install_int13h
	printStringLn "Int 13h installed for drive 82h"
	mov al, 0x82
	jmp .int_setup_done

	; Install a new int 13h handler for disk 0x83 (hd3)
.int_setup_hd3:
	mov dx, int13h_card_drive83_fixcount
	call install_int13h
	printStringLn "Int 13h installed for drive 83h"
	mov al, 0x83
	jmp .int_setup_done

	; Install a new int 13h handler for disk 0x80 (hd0) with
	; translation for already existing drive (i.e. drive 80 becomes
	; drive 81, etc)
.int_setup_hd0_translate:
	mov dx, int13h_card_drive80_translate
	call install_int13h
	printStringLn "Int 13h installed for drive 80h (translate)"
	call install_int19
	printStringLn "Int 19h installed"

	; Set a flag to let the int13,08 code know that it should
	; also adjust DL based on how many drives are present on
	; the original controller
	call memory_setTranslating

	mov al, 0x80
	jmp .int_setup_done




.int_setup_done:

	; Try communicating with a card. This is not really required here, it could
	; be done later by our int 18h implementation, but it provides early feedback, which is nice.
	printString "Init card.."
	mov cx, 3
.retry1:
	mov dl, '.'
	call putchar
	mov dl, al ; Drive number from above
	mov ah, 0
	push ds
	push ax
	int 13h
	pop ax
	pop ds
	jnc .init_ok

	call delay
	loop .retry1

	printStringLn "Not found"
	jmp .init_done

.init_ok:
	printStringLn "OK"
.init_done:
	call delay


.return:
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	retf




	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; get_nfdsks: Try to figure out if there is/are other disks present in the system.
	;
	; First check if the current int13h handler is the default PCjr BIOS handler.
	; If it is, the SD-Cart JR is the only drive.
	;
	; Otherwise, call reset disk system on each disk, checking status afterwards.
	; Count those not in error to know how many drives there are.
	;
	; Returns count in AX
	;
get_nfdsks:
	push es
	push bx
	push cx
	push dx
	push bp
	push di

	mov bp, 0 ; Counter for number of disks found

	xor ax, ax ; Segment 0000
	mov es, ax
	mov di, ax

	; Fetch current int13h
	es mov ax, [13h * 4]		; Offset
	es mov bx, [13h * 4 + 2]	; Segment

	cmp bx, 0xF000
	jne .thirdparty_handler
	cmp ax, 0xEC59
	jne .thirdparty_handler

.stock_handler:
	printStringLn "Current int13h handler: Standard"
	jmp .ret

.thirdparty_handler:
	printStringLn "Current int13h handler: 3rd party"


.disk_test:
	mov ah, 8
	mov dl, 0x80
	int 13h
	jc .disk_failed
	printString "Existing BIOS reports "
	call printHexByte ; From DL
	printStringLn " drive(s)"
	and dx, 0xff
	add bp, dx

.disk_failed:

.ret:
	mov ax, bp

	pop di
	pop bp
	pop dx
	pop cx
	pop bx
	pop es
	ret

%if 0
	; This address of the BDA is used for floppy related stuff by the
	; PCjr BIOS. So this does not work.

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; get_nfdsks: Get the number of fixed disks from the Bios Data Area, 40:75
	;
	; Returns number in AX
	;
get_nfdsks:
	push es
	mov ax, 0x0040		; Prepare segment. And AH=0 by the way.
	mov es, ax			; Set segment
	mov al, [es:0x75]	; Retrive byte.
	pop es
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; inc_nfdsks: Increase the number of fixed disks (BDA 40:75)
	;
inc_nfdsks:
	push ax
	push es
	mov ax, 0x0040		; Prepare segment.
	mov es, ax			; Set segment
	inc byte [es:0x75]	; Increase 40:75
	pop es
	pop ax
	ret
%endif


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_init_retry_delay : Delay for retries in sdinit
	;
card_init_retry_delay:
	push ax
	mov ax, 0xfff
.loop:
	dec ax
	nop
	nop
	nop
	jnz .loop
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; delay: Approximately 1s on a stock PCjr.
	;
	; Busy delay loops are not good, but in this case the environment is
	; pretty much fixed to PCjr only. Maybe this delay runs faster if the
	; user has upgraded the cpu to NEC V20, but precision is not critical.
	;
delay:
	push ax
	mov ax, 0xffff
.loop:
	dec ax
	nop
	nop
	nop
	jnz .loop
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; install_int13: Replace the current int 13h handler by our own.
	;
	; Arg: DX=New handler address. Assumed to be in the current code segment.
	;
	; The old handled is stored at NEWINT13 so it can be called when
	; int 13h is invoked to access other drives.
	;
install_int13h:
	push ax
	push bx
	push ds

	xor ax, ax	; Segment 0000
	mov ds, ax

	; Fetch current int13h
	mov ax, [13h * 4]		; Offset
	mov bx, [13h * 4 + 2]	; Segment

	; Move it
	mov [NEWINT13 * 4], ax
	mov [NEWINT13 * 4 + 2], bx

	; Now install our own handler
	mov word [13h * 4], dx
	mov [13h * 4 + 2], cs

	pop ds
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; panic: Restore video mode, display an hex value and hang there.
	;
	; BX : Value to display in the screen corner.
	;
	; Never returns.
	;
panic:
	cli
	mov ax, cs
	mov ds, ax

	push bx

	mov ah, 0
	mov al, 0
	int 10h

	; Cursor in upper left corner
	mov ah, 2
	mov bh, 0
	mov dh, 0
	mov dl, 0
	int 10h

	pop bx

	printString "Panic! "
	mov dx, bx
	call printHexWord

.noreturn:
	nop
	jmp .noreturn

	ret


section .data

	; Hack: Writing to cartridge will have no effect, but this is
	; not nice. int13h code should be fixed to remove the need
	; for tmpbuf.
tmpbuf: db 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0

