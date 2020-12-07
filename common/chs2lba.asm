
%ifndef NO_GEO2BLOCK
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; geo2block : Convert CHS values to a block number
	;
	; Input: CHS
	;
	; CX: Track and sector number
	; DH: Head number
	;
	; Also, unless FIXED_GEOMETRY is defined:
	;   DS:SI: Number of heads [word]
	;   DS:SI+2: Sectors per track [word]
	;
	; Output: 32-bit value in AH AL BH BL
	;                         31........0
	;
geo2block:
	push bp
	push cx
	push dx

	mov bp, cx	; Backup CX


	mov cx, bp	; restore original CX
	and cx, 0x3f; Isolate sector number bits
	dec cx		; convert to base 0
	push cx		; ; ; Stack 1: Sector number

	mov cx, dx	; Get Head number (DH)
	mov cl, ch
	xor ch, ch
	push cx		; ; ; Stack 2: Head number

	;	|F|E|D|C|B|A|9|8|7|6|5-0|  CX
	;	| | | | | | | | | | `----- sector number
	;	| | | | | | | | `---------  high order 2 bits of track/cylinder
	;	`------------------------  low order 8 bits of track/cyl number
	mov cx, bp	; restore original CX
	xchg ch, cl
	shr ch, 1
	shr ch, 1
	shr ch, 1
	shr ch, 1
	shr ch, 1
	shr ch, 1


	; Block number = (Track * Heads + H) * Sectors per track + Sector
	xor ax,ax
	xor bx,bx
	xor dx,dx

	mov ax, cx			; Load cylinder number in AX

%ifdef FIXED_GEOMETRY
	mov cx, GEO_HEADS
%else
	mov cx, [ds:si]	; Max 256
%endif
	mul cx				; Multiply by number of heads
	pop cx				; Retreive head number from stack
	add ax, cx			; Add to current address
	; note: Max 1024 sectors and 16 heads = 16384. Fits in AX, DX still zero.

%ifdef FIXED_GEOMETRY
	mov cx, GEO_SECTORS_PER_TRACK
%else
	mov cl, [ds:si + 2]	; Max 63
	xor ch, ch
%endif
	mul cx							; now this uses DX!
	pop cx							; Retreive Sector number
	add ax, cx						; Add sector
	adc dx, 0						;

	; AX and DX now contain the output value. But it must be AX:BX..
	mov bx, ax	; Least significant word in AX
	mov ax, dx	; Most significant workd in DX


	pop dx
	pop cx
	pop bp

	ret
%endif



