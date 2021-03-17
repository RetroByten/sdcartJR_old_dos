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
;;; File menu.asm
;;;
;;; Options menus
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

%define MENU_OPTION_NO_INSTALL		0
%define MENU_OPTION_SINGLE_DRIVE	1
%define MENU_OPTION_LAST_DRIVE		2
%define MENU_OPTION_FIRST_DRIVE		3


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Display the press a ket for options message with a count down.
	;
	; Returns with carry set on timeout
	;
menu_entry_common:
	push dx
	call newline
	printStringLn "* Press any key for options * "

	; * Press any key for options * 5 4 3 2 1

	call kb_flush

	mov dh, 5
.another_second:
	; Display countdown
	mov dl, dh
	call printHexNibble
	mov dl, ' '
	call putchar

	; Check for keypress
	call kb_wait_keypress_timeout
	jc .pressed

	dec dh
	jnz .another_second

.timeout:
	call newline
	stc
	jmp .ret
.pressed:
	call newline
	clc
.ret:
	pop dx
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Offer the option menu for setups with a single SD-Cart JR "hdd"
	;
	; Return choice (or default) in BL.
	;
offer_options_menu_single:
	push ax

	call menu_entry_common
	jc .ret_single_drive	; On timeout, return default.

.pressed:
	call newline
	printStringLn "0 - Do not install SD-Cart JR"
	printStringLn "1 - Install SD-Cart JR as single drive (default)"
.reask:
	call newline
	printString "Your choice? "
	call kb_get_key_echo

	; Range check
	cmp al, '0'
	jl .reask
	cmp al, '1'
	ja .reask

	cmp al, '1'
	jmp .ret_single_drive

	mov bl, MENU_OPTION_NO_INSTALL
	jmp .ret

.ret_single_drive:
	mov bl, MENU_OPTION_SINGLE_DRIVE

.ret:
	call newline
	call newline

	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Offer the option menu for setups with multiple drives.
	;
	; Return choice (or default) in BL.
	;
offer_options_menu_multidrive:
	push ax

	call menu_entry_common
	jc .ret_last_drive	; On timeout, return default.

.pressed:
	call newline
	printStringLn "0 - Do not install SD-Cart JR"
	printStringLn "1 - Install SD-Cart JR as single drive"
	printStringLn "2 - Install SD-Cart JR as last drive (default)"
	printStringLn "3 - Install SD-Cart JR as first drive"
.reask:
	call newline
	printString "Your choice? "
	call kb_get_key_echo

	; Range check
	cmp al, '0'
	jl .reask
	cmp al, '3'
	ja .reask

	; Convert to binary 0-3
	mov bl, al
	sub bl, '0'
	jmp .ret

.ret_last_drive:
	mov bl, MENU_OPTION_LAST_DRIVE

.ret:
	call newline
	call newline

	pop ax
	ret

