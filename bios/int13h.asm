; SD-Cart JR : PCJR Card reader cartridge
; Copyright (C) 2020-2021 Raphael Assenat <raph@raphnet.net>
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File int13h.asm
;;;
;;; Interrupt 13h (Disk services) implementation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bits 16
cpu 8086

section .text

%undef TRACE_INT
%define INIT_RETRIES	255


; Disk status codes
%define STATUS_NO_ERROR					0x00
%define STATUS_BAD_COMMAND				0x01
%define STATUS_SECTOR_NOT_FOUND			0x04
%define STATUS_FIXED_DISK_RESET_FAILED	0x05
%define STATUS_FIXED_DISK_DRV_NOT_READY	0xAA

; Fixed disk geometry
%define GEO_CYLINDERS	142
%define GEO_SECTORS_PER_TRACK	63
%define GEO_HEADS				16

;%define FIXED_GEOMETRY
%include 'chs2lba.asm'
%include 'chsfitter.asm'

%define CMOS_DRIVE_TYPE	0


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_card_drive80: Card int13h handler for drive 80
	;
int13h_card_drive80:
	pushf
	cmp dl, 0x80
	jne _int13h_common.not_disk
	jmp _int13h_common


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_card_drive81: Card int13h handler for drive 81
	;
int13h_card_drive81:
	pushf
	cmp dl, 0x81
	jne _int13h_common.not_disk
	jmp _int13h_common


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_card_drive81: Card int13h handler for drive 81, but with the ability
	; to correct the drive count in DL when int13,08 is invoked. (Otherwise
	; DOS/Fdisk won't look at drive 81)
	;
int13h_card_drive81_fixcount:
	pushf
	cmp dl, 0x80	; Floppy?
	jl _int13h_common.not_disk
	cmp dl, 0x81	; This drive?
	je _int13h_common
	cmp ah, 0x08	; Is this AH=8 Get drive parameters?
	jne _int13h_common.not_disk
	popf
	int NEWINT13
	inc dl	 ; Does not affect carry flag
	retf 2


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_card_drive80_translate: Card int13h handler for drive 80, with translation.
	;
	; I.e. The card has priority, the already installed drive will be at 81, but
	; this drive's BIOS does not know about that and still "thinks" it is drive 80
	; so subtract 1 before jumping to the old int13 handler.
	;
int13h_card_drive80_translate:
	pushf
	cmp dl, 0x80
	jl _int13h_common.not_disk	; Floppy
	jne .translate	; Not 80?
	jmp _int13h_common ; Ok this is for us!
.translate:
	jl .not_hdd
	dec dl
.not_hdd:
	jmp _int13h_common.not_disk


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_card_drive82: Card int13h handler for drive 82
	;
int13h_card_drive82:
	pushf
	cmp dl, 0x82
	jne _int13h_common.not_disk
	jmp _int13h_common


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_card_drive82_fixcount: See int13h_card_drive81_fixcount for comments.
	;
int13h_card_drive82_fixcount:
	pushf
	cmp dl, 0x80	; Floppy?
	jl _int13h_common.not_disk
	cmp dl, 0x82	; This drive?
	je _int13h_common
	cmp ah, 0x08	; Is this AH=8 Get drive parameters?
	jne _int13h_common.not_disk
	popf
	int NEWINT13
	inc dl	 ; Does not affect carry flag
	retf 2


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_card_drive83: Card int13h handler for drive 83
	;
int13h_card_drive83:
	pushf
	cmp dl, 0x83
	jne _int13h_common.not_disk
	jmp _int13h_common


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_card_drive82_fixcount: See int13h_card_drive81_fixcount for comments.
	;
int13h_card_drive83_fixcount:
	pushf
	cmp dl, 0x80	; Floppy?
	jl _int13h_common.not_disk
	cmp dl, 0x83	; This drive?
	je _int13h_common
	cmp ah, 0x08	; Is this AH=8 Get drive parameters?
	jne _int13h_common.not_disk
	popf
	int NEWINT13
	inc dl	 ; Does not affect carry flag
	retf 2


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; _int13h_common: Common int13h code past the initial drive ID check
	;
	; For use by the above entry points. Do not call directly.
	;
_int13h_common:
	sti
	cld
	push bp
	push ds

	; Restore our data segment
	mov bp, cs
	mov ds, bp

	; Range-check the function number.
	cmp ah, 0x1A
	jg int13_iret_stc	; Return with carry flag SET (error)

%ifdef TRACE_INT
	push dx
	mov dl, '['
	call putchar
	mov dl, ah
	call printHexByte
	mov dl, ']'
	call putchar
	pop dx
%endif

	; Arrange a jump to the correct routine
	mov bp, ax	; Backup AX
	xor al, al	; Clear AL
	xchg al, ah	; Mov AH (function no) to AL (AX now equals func. no)
	shl ax, 1	; Multiply by 2 (each ptr is a word)
	xchg ax, bp	; Restore AX, BP is now the function offset
	add bp, int13h_functions ; Add the base of the table

	jmp [cs:bp]	; Jump to it!

	; not reached

.not_disk:
	popf
	int NEWINT13
	retf 2

; Exit point with carry cleared.
int13_iret_clc:
%ifdef TRACE_INT
	push dx
	mov dl, 'c'
	call putchar
	pop dx
%endif
	pop ds
	pop bp
	popf
	clc
	retf 2

; Exit point with carry set.
int13_iret_stc:
%ifdef TRACE_INT
	push dx
	mov dl, 'C'
	call putchar
	pop dx
%endif
	pop ds
	pop bp
	popf
	stc
	retf 2

; Table of pointers for int13h functions
int13h_functions:
	dw int13h_fn00
	dw int13h_fn01
	dw int13h_fn02
	dw int13h_fn03
	dw int13h_fn04
	dw int13h_fn05
	dw int13h_fn06
	dw int13h_fn07
	dw int13h_fn08
	dw int13h_fn09
	dw int13h_fn0a
	dw int13h_fn0b
	dw int13h_fn0c
	dw int13h_fn0d
	dw int13h_fn0e
	dw int13h_fn0f
	dw int13h_fn10
	dw int13h_fn11
	dw int13h_fn12
	dw int13h_fn13
	dw int13h_fn14
	dw int13h_fn15
	dw int13h_fn16
	dw int13h_fn17
	dw int13h_fn18
	dw int13h_fn19
	dw int13h_fn1a


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn00 : Reset Disk System
	;
int13h_fn00:

	; if bit 7 is set, the diskette drive indicated by the lower 7 bits
	; will reset then the hard disk will follow; return code in AH is
	; for the drive requested
;	push ax
;	push dx
;	and dl, 0x7F
;	int NEWINT13
;	pop dx
;	pop ax

	push ax
	push bx
	push si
	push di
	push es
	; Card init returns the card size
	call card_init
	jc .skip_chs_compute

.compute_chs:
	; Put the returned card size on the stack
	push bx
	push ax
	mov si, sp	; 32bit block count argument for chs_fit
	sub sp, disk_geometry.size
	mov di, sp  ; STRUC disk_geometry pointer
	mov ax, ss  ; ES = SS
	mov es, ax
	call chs_fit

	; Displaying this only once at boot
	call memory_testCHSdisplayed
	jnz .no_chs_display
	call chs_display
	call newline
	call memory_setCHSdisplayed
.no_chs_display:
	; TODO : Store it for later use
	;

.store_chs:
	; Takes ES:DI, writes it to memory for use later
	call memory_saveCHS

	; Restore stack used by STRUC disk_geometry and
	; the card size
	add sp, disk_geometry.size
	pop ax
	pop bx

.skip_chs_compute:
	pop es
	pop di
	pop si
	pop bx
	pop ax

	jc .init_failed

.init_ok:
	push ds
		mov ax, 0x40
		mov ds, ax
		mov ah, STATUS_NO_ERROR
		mov [0x41], ah
	pop ds
	jmp int13_iret_clc

.init_failed:
	push ds
		mov ax, 0x40
		mov ds, ax
		mov ah, STATUS_FIXED_DISK_RESET_FAILED
		mov [0x41], ah
	pop ds
	jmp int13_iret_stc


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn01: Disk status
	;
	; returns the status byte located at 40:41 in the BIOS Data Area
	;
int13h_fn01:
	; Return with AL = status
	push ds
	mov ax, 0x40
	mov ds, ax
	mov al, [ds:0x41]
	pop ds
	mov ah, 0x01
	jmp int13_iret_clc

%ifdef TRACE_INT
trace_fn02_params:
	push dx
	mov dl, '>'
	call putchar
	mov dl, al
	call printHexByte
	mov dl, '|'
	call putchar
	mov dx, cx
	call printHexWord
	mov dl, '|'
	call putchar
	pop dx
	push dx
	call printHexWord
	mov dl, '<'
	call putchar
	pop dx
	ret

traceBlockNo:
	push dx
	mov dl, '{'
	call putchar
	mov dx, ax
	call printHexWord
	mov dx, bx
	call printHexWord
	mov dl, '}'
	call putchar
	pop dx
	ret
%endif


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn02: Read Disk Sectors
	;
	;	AL = number of sectors to read (1-128 dec.)
	;	CH = track/cylinder number  (0-1023 dec., see below)
	;	CL = sector number  (1-17 dec.)
	;	DH = head number  (0-15 dec.)
	;	DL = drive number (0=A:, 1=2nd floppy, 80h=drive 0, 81h=drive 1)
	;	ES:BX = pointer to buffer
	;
	; Return AH status, AL sectors read, CF...
	;
int13h_fn02:
	push bx
	push cx
	push di
	push si
	; DS and BP push/pop is taken care of in _int13h_common and common return code.

%ifdef TRACE_INT
	call trace_fn02_params
%endif

	; Save AX/BX before geo2block uses those. Must use registers that do
	; not contain adressing info..
	mov bp, bx ; Save destination offset to BP (for card_readSectors later)
	mov di, ax ; Save sector count

	; geo2block wants DS:SI pointing to number of heads (word)
	; followed by sectors per track (word).
	MEMORY_GETCHS ds, si
	add si, disk_geometry.heads ; Offset within STRUC disk_geometry
	; geo2block args: AL, CH, CL and DH
	call geo2block ; Return block no. in AX:BX

	; card_readSectors arguments
	;  AX:BX : Start sector computed by geo2block
	;  CX : Sector count
	;  DS:BP : Destination buffer
	mov cx, di ; Retreive saved AX
	and cx, 0xff ; Only keeep sector count (AL)
	mov si, es ; DS = ES
	mov ds, si
	; BP already = original BX
	call card_readSectors
	jc .drv_not_ready

.done:
	mov ax, di ; Restore original AX
	mov bx, 0x40
	mov ds, bx
	mov ah, STATUS_NO_ERROR
	mov [0x41], ah
	; AL still equals sector count
.return_clc:
	pop si
	pop di
	pop cx
	pop bx
	jmp int13_iret_clc

.drv_not_ready:

	; Retry the read. Helps for some cards where after init, the first sector reads
	; fails no matter what. Otherwise, when DOS boots and tries to look at the partitions,
	; the read fails and there is no drive c: in DOS.
	call card_readSectors
	jnc .done

	mov bx, 0x40
	mov ds, bx
	mov ah, STATUS_FIXED_DISK_DRV_NOT_READY
	mov [0x41], ah
	mov al, 0 ; no sectors were read

.return_stc:
	pop si
	pop di
	pop cx
	pop bx

	jmp int13_iret_stc



	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn03: Write Disk Sectors
	;
int13h_fn03:
	;	AL = number of sectors to write  (1-128 dec.)
	;	CX = track/cylinder number, sector number
	;	DH = head number  (0-15 dec.)
	;	DL = drive number
	;	ES:BX = pointer to buffer
	;
	;	Returns status in AH, written sector count in AL
	;	CF=0 on success
	push bx
	push cx
	push si
	push ds

%ifdef TRACE_INT
	call trace_fn02_params
%endif

	; Place sectors to write in CX. Clear CH and check for zero.
	and al, al
	jz .zero_write

.nonzero_count:
	push bx ; Save source buffer
	push ax	; Save sector count
	; geo2block wants DS:SI pointing to number of heads (word)
	; followed by sectors per track (word).
	MEMORY_GETCHS ds, si
	add si, disk_geometry.heads ; Offset within STRUC disk_geometry
	call geo2block ; Returns block no. in AX:BX
	pop cx ; retreive sector count (originally AX)
	and cx, 0xff ; keep only sector count
	; card_writeSectors args:
	;	CX : Sector count
	; 	ES:SI : Source buffer
	pop si ; Retreive source buffer (originally BX)
	call card_writeSectors
	jc .drv_not_ready

.done:
	mov cx, 0x40
	mov ds, cx
	mov ah, STATUS_NO_ERROR
	mov [0x41], ah
	; AL still equals requested sectors
.return_clc:
	pop ds
	pop si
	pop cx
	pop bx
	jmp int13_iret_clc

.zero_write:
	mov ah, STATUS_BAD_COMMAND
	jmp .error_common
.drv_not_ready:
	mov ah, STATUS_FIXED_DISK_DRV_NOT_READY
.error_common:
	mov cx, 0x40
	mov ds, cx
	mov [0x41], ah
	xor al, al ; no sectors were written
.return_stc:
	pop ds
	pop si
	pop cx
	pop bx
	jmp int13_iret_stc


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn04 : Verify disk sectors
	;
int13h_fn04:
	; TODO : Verify sectors by reading from the card and dropping the data?
	; For now, fake an instant success...

	; This call makes the led blink for a very short instant.
	call card_pulseLed

	mov ah, 0x00
	jmp int13_iret_clc

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn05: Format Disk Track
	;
	; AL = Interleave value
	; ES:BX = 512-byte format buffer
	; CX : Cylinder
	; DH : Head
	; DL : Drive
	;
int13h_fn05:
	push ax
	push bx
	push cx
	push dx
	push es
	push si

	and cx, ~0x1F	; Make sure "sector number" bits are ignored.
	or cx, 1		; Set sector number to 1 for geo2block

	; set ES:SI to STRUC disk_geometry
	call memory_getCHS
	mov dx, [es:si + disk_geometry.sectors]	; Retrive sectors per track

	; Setup a source buffer of zeroes ES:SI for card_writeSectors
	mov si, formatted_sector
	mov bx, ds
	mov es, bx

	call geo2block	; Compute block number and store in AX, BX

	mov cx, 1	; Sector count
	; DX = sectors_per_track
.format_loop:
	;  card_writeSectors args:
	;	AX..BX : Block number
	;   CX : Sector count
	;   ES:SI : Source buffer
	call card_writeSectors
	jc .error

	; Increment block number
	add bx, 1
	adc ax, 0

	dec dx
	jnz .format_loop

.ok:
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	; Return with success.
	mov ah, 0x00
	jmp int13_iret_clc
.error:
	pop si
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	; Return an error
	mov ah, 0xCC ; write fault
	jmp int13_iret_stc

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn06: FORMAT TRACK AND SET BAD SECTOR FLAGS
	; int13h_fn07: FORMAT DRIVE STARTING AT GIVEN TRACK
	;
int13h_fn06:
int13h_fn07:
	mov bx, ax
	jmp panic

	; Default for unimplemented or unsupported: Return with Carry
	jmp int13_iret_stc


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn08: Get Current Drive Parameters
	;
int13h_fn08:
	push es
	push si

	push ds
		mov ax, 0x40
		mov ds, ax
		mov ah, STATUS_NO_ERROR
		mov al, 0
		mov [0x41], ah
	pop ds
	mov ah, STATUS_NO_ERROR
	mov bl, CMOS_DRIVE_TYPE
%if 0
	mov cx, (((GEO_CYLINDERS-1) & 0xFF) << 8) | (((GEO_CYLINDERS-1) & 0x300) >> 2) | GEO_SECTORS_PER_TRACK
	mov dh, GEO_HEADS - 1	; returned value is max head number
%endif

	; Sets ES:SI to the location of STRUC disk_geometry
	call memory_getCHS

	mov cx, [es:si + disk_geometry.cylinders]
	dec cx ; We must return the maximum value, not the count
	xchg cl,ch ; CH to contain the low bits of cylinder count
	ror cl, 1 ; Cylinder count bits 9-10 (now CL 1-0) must be in CL bits 7-6
	ror cl, 1
	and cl, 0xC0
	or cl, [es:si + disk_geometry.sectors] ; Sectors per track (1-63)

	mov dh, [es:si + disk_geometry.heads]
	dec dh ; return value is max head number, not head count

	mov dl, 1	; Number of hard drives on first controller. (default value)

	;;; If the SD-Cart JR BIOS has taken over another hard drive BIOS, we must count it.
	call memory_testTranslating
	jz .not_translating

	; The the count reported by the other hard drive BIOS
	call int13h_add_other_drives_to_dx

.not_translating:

	; If SD-Cart JR is installed as drive 81h-83h, it needs to add the count of preceding
	; drives to DL
	call memory_testAdd0
	jz .no_add1
	inc dl

.no_add1:
	call memory_testAdd1
	jz .no_add2
	add dl, 2
.no_add2:

	pop si
	pop es

	jmp int13_iret_clc

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Add to DX the number of drives reported by int13h,08 DL=81h
	;
	; For use only when the SD-Cart BIOS is installed as drive 80h
	; in translation mode, otherwise recursion will happen.
	;
int13h_add_other_drives_to_dx:
	push bp

	mov bp, 0

	push ax
	push bx
	push cx
	push dx
	push es
	push di

	xor di, di
	mov es, di
	mov ah, 8
	mov dl, 0x81
	stc ; Set carry in advance, just in case int13h for drive 81 just returns.
	int 13h
	jc .err

	; More than 3 drives is suspicious. Likely DL was left as is?
	test dl, 0xfc
	jnz .err

	and dx, 0xff ; Number of drives in DL. clear DH.
	mov bp, dx

.err:

	pop di
	pop es
	pop dx
	pop cx
	pop bx
	pop ax

	; Intention is to add to DL (which is 1), but the value is in
	; BP. The upper byte has been cleared above to avoid touching DH (max head number)
	add dx, bp

	pop bp

	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; int13h_fn18: Set Media Type for Format
	;
int13h_fn18:
	;
	; Apparently this should be called before a format operation
	; to set the controller for the correct drive speed and
	; track-stepping option.
	;
	; Clearly not applicable to a flash card. Just ignore it.
	;
	mov ah, STATUS_NO_ERROR
	jmp int13_iret_clc


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Unhandled calls lead to panic
	;
int13h_fn09:
int13h_fn0a:
int13h_fn0b:
int13h_fn0c:
int13h_fn0d:
int13h_fn0e:
int13h_fn0f:
int13h_fn10:
int13h_fn11:
int13h_fn12:
int13h_fn13:
int13h_fn14:
int13h_fn15:
int13h_fn16:
int13h_fn17:
int13h_fn19:
int13h_fn1a:
	mov bx, ax
	jmp panic

	; Default for unimplemented or unsupported: Return with Carry
	jmp int13_iret_stc




section .data

	; Data written to "formatted" sectors
formatted_sector: times 512 db 0
