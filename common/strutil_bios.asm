; SD-Cart JR BIOS : BIOS for a PCJR SD Card reader cartridge
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
;;; File strutil_bios.asm
;;;
;;; String utilities using BIOS int 10h calls.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


%ifndef _strutil_bios_asm__
%define _strutil_bios_asm__

section .text

;;; hexdump
;
; BP: Pointer to data in DS
; CX: Length
;
hexdump:
	push ax
	push cx
	push ds
	push bp

	xor ax, ax
.next_byte:
	mov dl, [ds:bp]
	call printHexByte
	mov dl, ' '
	call putchar
	inc bp

	inc ax
	test ax, 7
	jne .noextraspace
	mov dl, ' '
	call putchar
.noextraspace:

	test ax, 15
	jne .nonewline
	call newline
.nonewline:

	loop .next_byte

	pop bp
	pop dx
	pop cx
	pop ax
	ret


; ***** printHexWord
; Word in dx
printHexWord:
	xchg dl, dh
	call printHexByte
	xchg dl, dh
	call printHexByte
	ret

; ***** printHexByte
; Byte in dl
printHexByte:
	push dx
	;shr dl, 4 ; Not available on 8086
	shr dl, 1
	shr dl, 1
	shr dl, 1
	shr dl, 1
	call printHexNibble
	pop dx
	call printHexNibble
	ret

; ***** printhexnibble
; Argument : Value in DL
printHexNibble:
	push dx
	and dl, 0FH
	cmp dl, 9
	ja letters
	add dl, '0'
	call putchar
	pop dx
	ret
letters:
	add dl, 'A'-10
	call putchar
	pop dx
	ret


; ******* putstring ******
; Argument : NUL-terminated string in DX
putstring:
	push ax
	push bx
	push si
	push bp

	mov si, dx
	cld

putstring_loop:
	lodsb
	cmp al,0
	jz putstring_done
	mov bx, 0x0001
	mov ah,0x0E		; int10 seems to destroy AH?
	int 10h
	jmp putstring_loop
putstring_done:

	pop bp
	pop si
	pop bx
	pop ax
	ret

;;; ******* newline *****
; Output newline characters
;
newline:
	push dx
	mov dl, 13
	call putchar
	mov dl, 10
	call putchar
	pop dx
	ret

; ******* putchar ******
; Argument: Character in DL
; Return: None
putchar:
	push ax
	push bx
	push bp
	mov ah,0EH
	mov al, dl
	mov bx, 0x0001
	int 10h
	pop bp
	pop bx
	pop ax
	ret

%macro printStringLn 1
section .data
%%str: db %1, 13, 10, 0
section .text
push dx
mov dx, %%str
call printStr
pop dx
%endmacro
%macro printString 1
section .data
%%str: db %1, 0
section .text
push dx
mov dx, %%str
call printStr
pop dx
%endmacro


%define printStr putstring

%endif
