; SD-Cart JR : PCJR Card reader cartridge
; Copyright (C) 2020 Raphael Assenat <raph@raphnet.net>
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
;;; File spi.asm
;;;
;;; SPI communication routines for the SD-Cart JR hardware
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cpu 8086
bits 16

section .text

; Cartridge uses CS2 (D0000-D7FFF):
; - Rom is visible from D000 to D5FF (24k)
; - Control logic is at 6000 to 7FFF:
;   - 6000-60FF : Exchange. Loads the output shift register with A0-A7. Returns
;               input shift register current value.
;   - 6100-61FF : Each reads generates one clock pulse.
;   - 6200-62FF : Selects the card (sets CS low)
;   - 6300-63FF : Deselects the card (sets CS high)
;
%define SPI_IO_SEGMENT	0xD600

; A 74ls139 monitors A8 and A9 to decide
; what operation is to be done when that
; memory is read.
%define OP_EXCHANGE_OFF	0x000
%define OP_SHIFT_OFF	0x100
%define OP_RESET_CS_OFF	0x200
%define OP_SET_CS_OFF	0x300


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_select: Drive the chip select output to a logic low
	;
	; No arguments
	; Nothing returned, no trashed registers.
	;
spi_select:
	push ax
	push ds

	mov ax, SPI_IO_SEGMENT
	mov ds, ax
	mov al, [OP_RESET_CS_OFF]

	pop ds
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_clk24_deselect_clk24: Send 24 clock pulses, deselect, send another 24 pulses
	;
	; This is a convenience function. It is sometimes useful (seems to avoid
	; some errors on some cards) to add extra clock cycles around deselction.
	;
	; No arguments
	; No return
	;
spi_clk24_deselect_clk24:
	push cx
	mov cx, 24
	call spi_justclock
	call spi_deselect
	call spi_justclock
	pop cx
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_select: Drive the chip select output to a logic high
	;
	; No arguments
	; Nothing returned, no trashed registers.
	;
spi_deselect:
	push ax
	push ds

	mov ax, SPI_IO_SEGMENT
	mov ds, ax
	mov al, [OP_SET_CS_OFF]

	pop ds
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_justclock: Transmit CX clock cycles, ignoring data.
	;
	; CX: Number of clock cycles to transmit. Maximum 256.
	; Nothing returned, no trashed registers.
	;
spi_justclock:
	push ax
	push cx
	push ds
	push si

	mov ax, SPI_IO_SEGMENT
	mov ds, ax

	; Set dummy byte value to transmit during clocking
	mov al, byte [0xff]
	mov si, OP_SHIFT_OFF

	rep lodsb

	pop si
	pop ds
	pop cx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; SPI_RECEIVE_BYTE : Transmit one dummy (0xFF) byte and receive one byte
	;
	; Arguments: None
	; Returns: Byte in AL. Garbage in AH.
	;
%macro SPI_RECEIVE_BYTE 0
	push ds
	push si

	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	; Load the dummy byte in the shift register
	mov al, [0xff]

	mov si, OP_SHIFT_OFF
	lodsw
	lodsw
	lodsw
	lodsw

	; Now retreive the byte that was shifted into
	; the 74ls164...
	mov al, [0xff]

	pop si
	pop ds
%endmacro


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_receive_byte : Transmit one dummy (0xFF) byte and receive one byte
	;
	; Arguments: None
	; Returns: Byte in AL. Garbage in AH.
	;
spi_receive_byte:
	SPI_RECEIVE_BYTE
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Macro SPI_SEND_BYTE : Transmit a word
	;
	; Arguments: Contant or indirect register (except SI)
	; Returns: nothing
	;
%macro SPI_SEND_BYTE 1
	push ax
	push ds
	push si

	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	mov si, OP_SHIFT_OFF

	; Perform a read access to OP_EXCHANGE to load the byte
	; to transmit in the 75ls165 shift register.
	;
	; The adress bits are the data.
	;
	mov al, [%1]

	lodsw
	lodsw
	lodsw
	lodsw

	pop si
	pop ds
	pop ax
%endmacro


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Macro SPI_SEND_WORD_BX : Transmit a word
	;
	; Arguments: Implied (BX)
	; Returns: nothing
	;
%macro SPI_SEND_WORD_BX 0
	push ax
	push bx
	push ds
	push si

	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	mov si, OP_SHIFT_OFF

	push bx

	; Perform a read access to OP_EXCHANGE to load the byte
	; to transmit in the 75ls165 shift register.
	;
	; The adress bits are the data.
	;
	mov bl,bh	; MSB first
	xor bh,bh
	mov al, [bx]

	lodsw
	lodsw
	lodsw
	lodsw

;	mov si, OP_SHIFT_OFF
	; Perform another read access to OP_EXCHANGE to load the second byte
	pop bx
	xor bh,bh	; LSB
	mov al, [bx]

	lodsw
	lodsw
	lodsw
	lodsw

	pop si
	pop ds
	pop bx
	pop ax
%endmacro


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_send_word : Transmit one word while discarding the received byte.
	;
	; Arguments:	BX is the value to be transmitted (MSB first)
	;
spi_send_word:
	SPI_SEND_WORD_BX
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_send_byte : Transmit one byte while discarding the received byte.
	;
	; Arguments:	BX is the value to be transmitted
	;
spi_send_byte:
	push ax
	push bx
	push ds
	push si

	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	; Perform a read access to OP_EXCHANGE to load the byte
	; to transmit in the 75ls165 shift register.
	;
	; The adress bits are the data.
	;
	xor bh,bh
	mov al, [bx]

	mov si, OP_SHIFT_OFF

	; 15us
	lodsw
	lodsw
	lodsw
	lodsw

	pop si
	pop ds
	pop bx
	pop ax
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_exchange_byte: Transmits byte in BX. Returns received value in AL.
	;
	; Destroys AH.
	;
spi_exchange_byte:
	push bx
	push si
	push ds

	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	; Perform a read access to OP_EXCHANGE to load the byte
	; to transmit in the 75ls165 shift register.
	;
	; The adress bits are the data.
	;
	xor bh,bh
	mov al, [bx]

	mov si, OP_SHIFT_OFF

	; 15us
	lodsw
	lodsw
	lodsw
	lodsw

	; Now retreive the byte that was shifted into
	; the 74ls164...
	mov al, [0xff]

	pop ds
	pop si
	pop bx
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_rxbytes : Receive a fixed number of bytes while transmitting 1s.
	;
	; CX: Byte count
	; Destination is ES:DI
	;
spi_rxbytes:
	push ax
	push bx
	push cx
	push dx
	push es
	push ds
	push si
	push di

	; Set DS to cartridge segment, so lodsw is usable for clocking
	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	; Load dummy byte for first 8 cycles.
	mov al, [0xff]

	mov bx, OP_SHIFT_OFF + 7
	mov dx, 2
	std
.next_byte:
	mov si, bx	; This starts at SI=7
	lodsw		; SI=105 when complete
	lodsw		; SI=103 when complete
	lodsw		; SI=101 when complete
	lodsw		; SI=0FF when complete.

	; Below 0x100, there is no clocking. But memory accesses from 0-FF loads
	; new value in the transmit shift register. But that's good, SI equals FF now
	; so it loads 0xFF. Eactly what's wanted!
	;
	movsb	; Copy received byte, load FF for next transmission.
	add di, dx	; 3 cycles vs 4 (std and cld combo)
	loop .next_byte

	cld		; Clear decrement.

	pop di
	pop si
	pop ds
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	ret


%ifndef NO_UNROLL_READ512
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_rx512bytes : Receive a fixed number of bytes while transmitting 1s.
	;
	; Destination is ES:DI
	;
spi_rx512bytes:
	push ax
	push bx
	push es
	push ds
	push si
	push di

	; Set DS to cartridge segment, so lodsw is usable for clocking
	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	; Load dummy byte for first 8 cycles.
	mov al, [0xff]

	mov bx, OP_SHIFT_OFF + 7
	std

%rep 512
	mov si, bx	; This starts at SI=7
	lodsw		; SI=105 when complete
	lodsw		; SI=103 when complete
	lodsw		; SI=101 when complete
	lodsw		; SI=0FF when complete.

	; Below 0x100, there is no clocking. But memory accesses from 0-FF loads
	; new value in the transmit shift register. But that's good, SI equals FF now
	; so it loads 0xFF. Eactly what's wanted!
	;
	movsb	; Copy received byte, load FF for next transmission.
	inc di
	inc di
%endrep

	cld		; Clear decrement.

	pop di
	pop si
	pop ds
	pop es
	pop bx
	pop ax
	ret
%endif


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_txbytes : Transmit a number of bytes, ignoring reception
	;
	; BP: Pointer to data (in DS)
	; CX: Bytes to transmit
	;
spi_txbytes:
	push ax
	push bx
	push cx
	push dx
	push bp
	push es
	push ds
	push si

	; Setup ES so buffer is at ES:BP
	mov ax, ds
	mov es, ax

	; Set DS to cartridge segment, so lodsw is usable for clocking
	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	mov dx, OP_SHIFT_OFF

	xor bh, bh
.next_byte:
	mov bl, [es:bp]	; Retrive byte to transmit
	inc bp			; Advance to next buffer position
	mov bl, [bx]	; Access in 00-ff range based on value to load shift register

	; Clock 8 bytes (each lodsw generates two pulses)
	mov si, dx	; This is to prevent clocking from no longer working after 256 cycles...
	lodsw
	lodsw
	lodsw
	lodsw

	loop .next_byte

	pop si
	pop ds
	pop es
	pop bp
	pop dx
	pop cx
	pop bx
	pop ax
	ret

%ifndef NO_UNROLL_WRITE512

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; spi_tx512bytes : Transmit 512 bytes, ignoring reception
	;
	; BP: Pointer to data (in DS)
	;
spi_tx512bytes:
	push ax
	push bx
	push cx
	push dx
	push bp
	push es
	push ds
	push si

	; Setup ES so buffer is at ES:BP
	mov ax, ds
	mov es, ax

	; Set DS to cartridge segment, so lodsw is usable for clocking
	mov ax, SPI_IO_SEGMENT + (OP_EXCHANGE_OFF >> 4)
	mov ds, ax

	mov dx, OP_SHIFT_OFF

	xor bh, bh

	mov cx, 32
.next:
	; Reposition SI periodically to avoid going over 0xff
	mov si, dx
%rep 16
	mov bl, [es:bp]	; Retrive byte to transmit
	inc bp			; Advance to next buffer position
	mov bl, [bx]	; Access in 00-ff range based on value to load shift register

	; Clock 8 bytes (each lodsw generates two pulses)
	lodsw
	lodsw
	lodsw
	lodsw
%endrep
	dec cx
	jnz .next

	pop si
	pop ds
	pop es
	pop bp
	pop dx
	pop cx
	pop bx
	pop ax
	ret



%endif
