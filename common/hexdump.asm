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


