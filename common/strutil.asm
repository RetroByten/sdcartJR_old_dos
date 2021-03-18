;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File strutil.asm
;;;
;;; String utilities using DOS int 21h calls.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


%ifndef _strutil_asm__
%define _strutil_asm__

section .text

; ***** printSegmentOffset
; Segment in BX
; Offset in AX
printSegmentOffset:
	push dx
	mov dx, bx
	call printHexWord
	mov dl, ':'
	call putchar
	mov dx, ax
	call printHexWord
	pop dx
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
; Argument : NUL-terminated string in DX, max output chars in CX
putstring_n:
	push ax
	push bx
	push dx

	mov bx,dx
	mov ah,02H
.loop:
	and cx, cx
	je .done
	dec cx
	mov dl, [bx]
	cmp dl,0
	jz .done
	int 21h
	inc bx
	jmp .loop
.done:

	pop dx
	pop bx
	pop ax
	ret


; ******* putstring ******
; Argument : NUL-terminated string in DX
putstring:
	push ax
	push bx
	push dx

	mov bx,dx
	mov ah,02H
putstring_loop:
	mov dl, [bx]
	cmp dl,0
	jz putstring_done
	int 21h
	inc bx
	jmp putstring_loop
putstring_done:

	pop dx
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
	mov ah,02H
	int 21h
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
