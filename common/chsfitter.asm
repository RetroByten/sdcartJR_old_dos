;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File chsfitter.asm
;;;
;;; Helpers for finding CHS values given a number of blocks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


STRUC disk_geometry
	.cylinders: resw 1
	; Also used with geo2block, order is important
	.heads: resw 1
	.sectors: resw 1
	.size:
ENDSTRUC


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; chs_display : Display disk geometry uing format "CHS=%d,%d,%d\n"
	;
	; Input ES:DI   : Pointer to struct disk_geometry
	;
	; Returns nothing.
	;
chs_display:
	push cx

	printString "CHS="
	mov cx, [es:di + disk_geometry.cylinders]
	call printInt16
	printString ","
	mov cx, [es:di + disk_geometry.heads]
	call printInt16
	printString ","
	mov cx, [es:di + disk_geometry.sectors]
	call printInt16
	call newline

	pop cx
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; chs_fit: Find CHS geometry fitting card size (given in blocks)
	;
	; Input:
	;         ES:DI   : Pointer to struct disk_geometry
	;         ES:SI   : Pointer to 32bit block count
	;
	; Output: Modifies structure ES:DI points to with result.
	;
	; Return with CF set if fitting failed
	;
chs_fit:
	push ax
	push cx
	push dx

	; Start with 1 head, 1 sector per track
	mov word [es:di + disk_geometry.heads], 1
	mov word [es:di + disk_geometry.sectors], 1

.fit_cylinders:
	;;; quick test to reject head/sector counts which even with 1024 cylinders cannot
	; cover all the card capacity.
	;
	; This saves a lot of iterations.
	mov cx, 1024
	mov ax, [es:di + disk_geometry.sectors]
	mul cx
	mul word [es:di + disk_geometry.heads]
	; Check if the block count is < than target
	cmp dx, [es:si + 2]
	jb .oversize
	ja .filled
	cmp ax, [es:si]
	jb .oversize

	; If we are here, it means that with 1024 cylinders are enough
	; to cover the capacity of the card
.filled:

	; Increment track count until a match
	; is found or track count > 1024
	mov cx, 1	; Track count
.next:

	; Compute the device size based on current CHS
	;
	; cx * [geo_heads] * [geo_sectors_per_track]
	; DX:AX = [geo_sectors_per_track] * CX
	mov ax, [es:di + disk_geometry.sectors]
	mul cx
	; geo_sectors_per_track max. is 63, and CX max is 1023.
	; 63 * 1023 = 64449, therefore DX always 0.
	; DX:AX = ax * [geo_heads]
	mul word [es:di + disk_geometry.heads]

	; Now DX:AX corresponds to the block count obtained
	; by current CHS parameters.

	; Check for equality
	cmp ax, [es:si]
	jne .not_equal
	cmp dx, [es:si + 2]
	je .found
.not_equal:

	; Check if the block count is > than target
	cmp dx, [es:si + 2]
	ja .oversize
	jb .not_oversize
	cmp ax, [es:si]
	ja .oversize

.not_oversize:
	inc cx
	cmp cx, 1024
	jbe .next


	; Oversize or does not fit
.oversize:
	; Double head count
	shl byte [es:di + disk_geometry.heads], 1
	jnc .fit_cylinders
	; > 255? restart with 1 head, double sectors per track
	mov word [es:di + disk_geometry.heads], 1
	shl word [es:di + disk_geometry.sectors], 1
	; > 1024?
	cmp word [es:di + disk_geometry.sectors], 63
	ja .error

	jmp .fit_cylinders

.found:
	; Store the determined cylinder count
	mov [es:di + disk_geometry.cylinders], cx



	clc
	jmp .done

	; This would happen for very large cards (> 8GB) which cannot be supported
	; through the basic int 13h interface.
.error:
	stc

.done:
	pop dx
	pop cx
	pop ax
	ret


