
; I don't know which memory I can use, so I am abusing the interrupt vector
; table.
;
; According to Ralph's brown interrupt list, int 4d-4f are not used by
; things which I *think* could be found on a PCjr?

%define INT_USED_TO_STORE_DATA	0x4D

; The four bytes there are used as such.
;
; [0] : Flags
;       - Bit 0: CMD1 used
;
; [1-3] : Not used

%define MEMFLAG_CMD1	0x01

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; mem_clearFlags : Clear all the flags.
	;
	;
mem_clearFlags:
	push ax
	mov al, 0x00
	call mem_setFlags
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; mem_setFlags : Save one byte of flags to a memory location
	;
	; AL : New value for flags
	;
mem_setFlags:
	push ds

	push ax
	xor ax, ax	; Segment 0000
	mov ds, ax
	pop ax

	mov [INT_USED_TO_STORE_DATA * 4], al

	pop ds
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; mem_getFlags : Retrieve one byte of flags from a memory location
	;
	; Return:
	;
	; 	AH : Clearned
	; 	AL : Flags
	;
mem_getFlags:
	push ds
	xor ax, ax	; Segment 0000
	mov ds, ax
	mov al, [INT_USED_TO_STORE_DATA * 4]
	pop ds
	ret


; card_io.asm requires some macros to store information
; about the card type and features. How those are stored
; is application specific.
%macro JMP_CARD_IO_FLAG_SET 2
	push ax
	push ds
	test byte [INT_USED_TO_STORE_DATA * 4], %1
	pop ds
	pop ax
	jnz %2
%endmacro
%macro SET_CARD_IO_FLAG 1
	push ax
	push ds
	xor ax, ax	; Segment 0000
	mov ds, ax
	or byte [INT_USED_TO_STORE_DATA * 4], %1
	pop ds
	pop ax
%endmacro
%macro CLR_CARD_IO_FLAGS 0
	push ax
	push ds
	xor ax, ax	; Segment 0000
	mov ds, ax
	mov byte [INT_USED_TO_STORE_DATA * 4], 0
	pop ds
	pop ax
%endmacro

