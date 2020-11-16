;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File mbr.asm
;;;
;;; Helpers for reading and interpreting the MBR
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bits 16
cpu 8086


section .text


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; mbr_read : Read the 512-byte MBR
	;
	; Argument: Destination buffer (min. 512 bytes) in DS:BP
	;
	; Requires the card be already initialized and ready
	;
mbr_read:
	push ax
	push bx
	push cx
	xor ax, ax  ; Address bits 31-16
	xor bx, bx  ; Address bits 15-0
	mov cx, 1
	call card_readSectors
	pop cx
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; mbr_getPartitionOffset : Find the offset (block address) of the first
	; partition with a valid status and non-zero type
	;
	; Argument: DS:SI pointing to MBR data
	;
	; Returns:  - LBA of first absolute sector in AX:BX (AX high word, BX low word)
	;           - Partition type in DL
	;           - Carry set on error
	;
mbr_getPartitionOffset:
	push cx
	push si

	mov cx, 4
	add si, 0x1BE	; Offset for the first partition entry

.next_partition:
	; Check for a valid status
	mov al, [si + 0]	; Status
	and al, 0x7F	; 80 Bootable, 00 inactive, 01-7F invalid
	jnz .skip
	; Check for a non-zero type
	mov al, [si + 4]	; Partition type
	and al, al
	jz .skip

	; Ok, requirements met!
	mov dl, al	;	Partition type returned in DL
	; Fill AX/BX with the first absolute sector
	mov bx, [si + 0x8]
	mov ax, [si + 0xA]
	clc	; Indicate success
	jmp .done

.skip:
	add si, 16		; Advance to next partition
	loop .next_partition

	stc ; Error

.done:
	pop si
	pop cx
	ret

