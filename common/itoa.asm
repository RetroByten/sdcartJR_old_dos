
	; CX : Number
	; DS:BX : Destination string
itoa:
	push ax
	push bx
	push cx
	push dx
	push es
	push di

	mov ax, ds
	mov es, ax
	mov di, bx

	cmp cx, 10
	jl _itoa_less_than_10
	cmp cx, 100
	jl _itoa_less_than_100
	cmp cx, 1000
	jl _itoa_less_than_1000
	cmp cx, 10000
	jl _itoa_less_than_10000

_itoa_less_than_100000:
	mov ax, cx
	mov bx, 10000
	mov dx, 0
	div bx
	mov cx, ax

	add cx, '0'
	mov ax, cx
	stosb

	mov cx, dx

_itoa_less_than_10000:
	mov ax, cx
	mov bx, 1000
	mov dx, 0
	div bx
	mov cx, ax

	add cx, '0'
	mov ax, cx
	stosb

	mov cx, dx

_itoa_less_than_1000:
	mov ax, cx
	mov bx, 100
	mov dx, 0
	div bx
	mov cx, ax

	add cx, '0'
	mov ax, cx
	stosb

	mov cx, dx

_itoa_less_than_100:
	mov ax, cx
	mov bx, 10
	mov dx, 0
	div bx
	mov cx, ax

	add cx, '0'
	mov ax, cx
	stosb

	mov cx, dx
_itoa_less_than_10:
	add cx, '0'
	mov ax, cx
	stosb

_itoa_done:
	mov al, 0	; Zero termination
	stosb

	pop di
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret


