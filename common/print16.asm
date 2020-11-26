;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File print16.asm
;;;
;;; Function to print a 16 bit integer in decimal using putchar(dx)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


%ifndef _print16_asm__
%define _print16_asm__

%macro doPosition 1
	mov ax, cx
	mov bx, %1
	mov dx, 0
	div bx

	push dx
		mov dx, ax
		add dx, '0'
		call putchar
	pop dx

	mov cx, dx

%endmacro


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; printInt16: Print a 16 bit integer in decimal
	;
	;	CX: Integer
	;
	; Does not output a newline.
	;
printInt16:
	push ax
	push bx
	push cx
	push dx

	cmp cx, 10
	jl .less_than_10
	cmp cx, 100
	jl .less_than_100
	cmp cx, 1000
	jl .less_than_1000
	cmp cx, 10000
	jl .less_than_10000

.less_than_100000:
	doPosition 10000
.less_than_10000:
	doPosition 1000
.less_than_1000:
	doPosition 100
.less_than_100:
	doPosition 10
.less_than_10:
	add cx, '0'
	mov dx, cx
	call putchar

.done:
	pop dx
	pop cx
	pop bx
	pop ax
	ret

%unmacro doPosition 1

%endif
