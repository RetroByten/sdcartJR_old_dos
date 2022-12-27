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
	; chs_display : Display disk geometry uing format "CHS=%d,%d,%d"
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
	; Return with CF set if the result is not perfect (i.e. some lost
	; sectors since no combination of cylinders, heads and sector count
	; resulted in the wanted total number of blocks, or if the device
	; is larger than what can be reprensed withing the int13h CHS geometry
	; limits (1024 cylinders, 255* heads and 63 sectors per track)
	;
%define CHSFIT_MIN_HEADS	2
	; * Fdisk writes incorrect data in the partition table with only one head. For
	; instance, a parition on a 8MB device will be CHS 0,1,1 - 829,0,16
	;                                                    ^
	;                                                    H=1? above 0!
%define CHSFIT_MAX_HEADS	255
	; * reportedly, DOS and Win95 cannot handle 256 heads.
	;
chs_fit:
	push ax
%ifdef FIXED_GEOMETRY ; Retrobyten - Set CHS geometry to fixed values
	mov ax,GEO_CYLINDERS
	mov [es:di + disk_geometry.cylinders],ax
	mov ax,GEO_SECTORS_PER_TRACK
	mov [es:di + disk_geometry.sectors],ax
	mov ax,GEO_HEADS
	mov [es:di + disk_geometry.heads],ax
%else
	push cx
	push dx

	; Start with 2 heads, 1 sector per track
	mov word [es:di + disk_geometry.heads], CHSFIT_MIN_HEADS
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
	jb .does_not_fit1024
	ja .filled
	cmp ax, [es:si]
	jb .does_not_fit1024

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

	jmp .does_not_fit

.does_not_fit1024:
	cmp word [es:di + disk_geometry.sectors], 63
	jne .does_not_fit	; Do not stop until number of sectors is maxed.
	cmp word [es:di + disk_geometry.heads], CHSFIT_MAX_HEADS
	jne .does_not_fit	; Do not stop until number of heads is maxed
;	printStringLn "Maxed"
	jmp .found_maxed

.oversize:
	cmp word [es:di + disk_geometry.sectors], 63
	jne .does_not_fit	; Do not stop until number of sectors is maxed.
	cmp word [es:di + disk_geometry.heads], CHSFIT_MAX_HEADS
	jne .does_not_fit	; Do not stop until number of heads is maxed

	; This means we just went past the number of cylinders
	; that fitted. Decrese it to get a count that does not
	; overflow the device.
	dec cx
;	printStringLn "Imperfect"
	jmp .found_imperfect

	; Oversize or does not fit
.does_not_fit:
	; Double head count
	shl word [es:di + disk_geometry.heads], 1
	cmp word [es:di + disk_geometry.heads], CHSFIT_MAX_HEADS + 1
	jne .skip255
	mov word [es:di + disk_geometry.heads], CHSFIT_MAX_HEADS
.skip255:
	cmp word [es:di + disk_geometry.heads], CHSFIT_MAX_HEADS
	jbe .fit_cylinders
	; > 255? restart with 1 head, double sectors per track
	mov ax, [es:di + disk_geometry.sectors]
	mov word [es:di + disk_geometry.heads], ax

	shl word [es:di + disk_geometry.sectors], 1
	cmp word [es:di + disk_geometry.sectors], 64
	jne .fit_cylinders
	mov word [es:di + disk_geometry.sectors], 63
	jmp .fit_cylinders


.found_maxed:
.found_imperfect:
	; Store the determined cylinder count
	mov [es:di + disk_geometry.cylinders], cx
	stc	; Indicate a non-exact match
	jmp .done

.found:
	; Store the determined cylinder count
	mov [es:di + disk_geometry.cylinders], cx
	clc

.done:

	; Try to use more sectors than heads, as formatting is done one head at a time, and
	; formatting a device with 128 heads and 1 sector per track is annoying. Especially
	; if there are 900 or so cylinders...
.maximize_sectors:
	cmp word [es:di + disk_geometry.heads], CHSFIT_MIN_HEADS
	jbe .finish
	; Check how many sectors per track we would have after doubling
	mov ax, [es:di + disk_geometry.sectors]
	shl ax, 1
	cmp ax, 63 ; More than 63? Stop now.
	jg .finish
	shr word [es:di + disk_geometry.heads], 1	; Half the number of heads
	shl word [es:di + disk_geometry.sectors], 1	; Double the number of sectors
	jmp .maximize_sectors

.finish:

	pop dx
	pop cx
%endif 
	pop ax
	ret


