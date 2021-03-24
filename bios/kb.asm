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
;;; File kb.asm
;;;
;;; Keyboard support functions, used by menus
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Wait for a keypress, with a timeout of approx. 1s.
	;
	; Returns with carry set if a key was pressed. Does not remove the keypress
	; from the buffer.
	;
kb_wait_keypress_timeout:
	push ax
	push bx

	mov bx, 7000
.loop:
	mov ah, 0x01	; Get keyboard status
	int 16h
	jnz .press
	dec bx
	jnz .loop
.timeout:
	clc
	jmp .ret
.press:

	stc
.ret:
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Read all keypress from the keyboard buffer
	;
kb_flush:
	push ax

.again:
	; Check status
	mov ah, 0x01
	int 16h
	jz .empty

	; Read character
	mov ah, 0x00
	int 16h
	jmp .again

.empty:
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Wait for one character from the keyboard
	;
	; Ascii value returned in AL, scan code in AH.
	;
kb_get_key_echo:
	push bx

	mov ah, 0x00
	int 16h
	and al, al
	jz .nul

	mov bx, 0x01
	mov ah, 0xe
	int 10h
.nul:

	pop bx
	ret
