
; I don't know which memory I can use, so I am abusing the interrupt vector
; table.
;
; According to Ralph's brown interrupt list, int 4d-4f are not used by
; things which I *think* could be found on a PCjr?

%define INT_USED_TO_STORE_DATA	0x4D

; The eight bytes there are used as such.
;
; [0] : Flags (see card_io.asm)
; [1] : Other status
;       Bit 0: CHS displayed
;
; [2-3] : cylinders
; [4-5] : heads
; [6-7] : sectors per track
; The above matches STRUC disk_geometry

%define MEM_STATUS_CHS_DISPLAYED	1

memory_testCHSdisplayed:
	push ax
	push ds
	xor ax, ax	; Segment 0000
	mov ds, ax
	test byte [INT_USED_TO_STORE_DATA * 4 + 1], MEM_STATUS_CHS_DISPLAYED
	pop ds
	pop ax
	ret

memory_setCHSdisplayed:
	push ax
	push ds
	xor ax, ax	; Segment 0000
	mov ds, ax
	or byte [INT_USED_TO_STORE_DATA * 4 + 1], MEM_STATUS_CHS_DISPLAYED
	pop ds
	pop ax
	ret

	; ES:DI -> Memory
memory_saveCHS:
	push ax
	push ds
	xor ax, ax
	mov ds, ax
%assign off 0
%rep 3
	mov ax, [es:di + off * 2]
	mov [INT_USED_TO_STORE_DATA * 4 + 2 + off * 2], ax
%assign off off+1
%endrep
	pop ds
	pop ax
	ret


	; Set ES:SI to location of STRUC disk_geometry
memory_getCHS:
	mov si, 0
	mov es, si
	mov si, INT_USED_TO_STORE_DATA * 4 + 2
	ret

	;; Arguments SegmentReg, OffsetReg
%macro MEMORY_GETCHS 2
	mov %2, 0
	mov %1, %2
	mov %2, INT_USED_TO_STORE_DATA * 4 + 2
%endmacro


; card_io.asm requires some macros to store information
; about the card type and features. How those are stored
; is application specific.
%macro JMP_CARD_IO_FLAG_SET 2
	push ax
	push ds
	xor ax, ax	; Segment 0000
	mov ds, ax
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
%macro CLR_CARD_IO_FLAG 1
	push ax
	push ds
	xor ax, ax	; Segment 0000
	mov ds, ax
	and byte [INT_USED_TO_STORE_DATA * 4], ~(%1)
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

