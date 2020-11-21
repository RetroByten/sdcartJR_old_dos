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
;;; File card_cmd.asm
;;;
;;; Collection of functions for sending card commands.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cpu 8086
bits 16

section .text

%define CARD_READ_OK_TOKEN	0xFE

; The card normally sends 0xFF a number of 0xff values (0 to 8 bytes)
; and then the R1 response.
;
; How many bytes should be read waiting for R1 before giving up.
;
%define CARD_MAX_BYTES_UNTIL_R1 10

%define CARD_MAX_BYTES_UNTIL_DATA_TOKEN	10000
%define CARD_MAX_BYTES_UNTIL_DATA_RESPONSE_TOKEN	10000
%define CARD_MAX_BYTES_WAIT_IDLE	2000

%define CARD_MAX_BUSY_BYTES 1024


%ifdef TRACE_ERRORS
%macro ERR_TRACE 1
push dx
mov dl, 'E'
call putchar
mov dl, %1
call putchar
mov dl, ' '
call putchar
pop dx
%endmacro
%else
%macro ERR_TRACE 1
%endmacro
%endif



	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd0 : Send CMD0 and check for R1 reply.
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd0:
	push bp
	mov bp, _dat_cmd0
	call card_sendCMD_R1
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd1 : Send CMD1 and check for R1 reply.
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd1:
	push bp
	mov bp, _dat_cmd1
	call card_sendCMD_R1
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd59 : Disable CRC
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd59:
	push bp
	mov bp, _dat_cmd59
	call card_sendCMD_R1
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd12 : Stop transmission
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd12:
	push bp
	push ds
	mov bp, cs
	mov ds, bp
	mov bp, _dat_cmd12
	call card_sendCMD_R1B
	pop ds
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd16 : Set block length to 512
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd16:
	push bp
	mov bp, _dat_cmd16
	call card_sendCMD_R1
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd9 : Send CSD
	;
	; ES:DI : Destination buffer (16 bytes)
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd9:
	push bp
	mov bp, _dat_cmd9
	call card_sendCMD_R1_D16
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd10 : Send CMD10 (SEND_CID) and check for R1 reply and data.
	;
	; ES:DI : Destination buffer (16 bytes)
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd10:
	push bp
	mov bp, _dat_cmd10
	call card_sendCMD_R1_D16
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_sendCMD_R1_D16 : Send 6-byte command, check for R1 reply and 16 data bytes.
	;
	; BP : Command (6 bytes)
	; ES:DI : Destination buffer (16 bytes)
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_sendCMD_R1_D16:
	call spi_select

	push bx
	push cx

	; Transmit command
	;	mov bp, _dat_cmd10
	mov cx, 6
	call spi_txbytes

	; The cards sends 0xff until it finally gives R1. R1
	; is easily detected by looking at the most significant bit
	; that shall be 0.
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.readanotherbyte:
	call spi_receive_byte ; returns in AL

	and al, al
	jns .got_r1	; Ah ha!
	loop .readanotherbyte

	; Oups, R1 never came our way. Return with carry set...
	call spi_deselect
	ERR_TRACE '5'

	stc
	jmp .done

.got_r1:
	; Now a number of 0xFF should follow, and then there should
	; be a data token (0xFE), followed by a data block of 16 bytes,
	; followed by a 2-byte CRC.

	mov cx, CARD_MAX_BYTES_UNTIL_DATA_TOKEN
.readanotherbyte2:
	call spi_receive_byte ; returns in AL

	cmp al, 0xFE
	je .got_token	; Ah ha!

	; Error token is 000xxxxx format
	test al, 0xE0
	jz .error_token

	loop .readanotherbyte2

	; Oups, token never came our way. Return with carry set...
	ERR_TRACE '6'
	call spi_deselect
	stc
	jmp .done

.error_token:

	; Oups, there was an error...
	call spi_deselect
	stc
	jmp .done

.got_token:

	mov cx, 16
	; Now uses ES:DI
	call spi_rxbytes

	; Read the CRC (ignoring it for now) and one more byte.
	mov cx, 3*8
	call spi_justclock

	call spi_deselect

	mov cx, 3*8
	call spi_justclock

	clc

.done:
	pop cx
	pop bx
	ret





	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd55 : Send CMD55 and check for R1 reply.
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd55:
	push bp
	mov bp, _dat_cmd55
	call card_sendCMD_R1
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_acmd41 : Send CMD55 followed by CMD41, checking for R1 replies
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_acmd41:
	push bp

	mov bp, _dat_cmd55
	call card_sendCMD_R1
	jc .error
	cmp al, 0x01
	je .cmd55_ok
	cmp al, 0x00
	je .cmd55_ok
	jmp .error

.cmd55_ok:
	mov bp, _dat_amcd41
	call card_sendCMD_R1

.error:
	pop bp
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd17 : Send CMD17 (Single Block Read) and check for reply
	;
	; Arguments:	BP to buffer of at least 512 bytes
	;				AX to bits 31-16 of block number
	;				BX to bits 15-0 of block number
	; Return: Writes to buffer, return with the data or error token in AL
	;         (0xFE is the OK value)
	;
	; Returns with carry set on timeout
	;
card_cmd17:
	call spi_select

	push bx
	push cx
	push dx
	push bp

	; Send CMD17
	mov dx, bx	; copy BX to DX, since BX is the argument for spi_send_X

	SPI_SEND_BYTE 0x40 + 17

	mov bx, ax
	call spi_send_word
	mov bx, dx	; Get original BX values
	call spi_send_word

	SPI_SEND_BYTE 0xff	; CRC

	; The cards sends 0xff until it finally gives the first byte
	; of R7, which is identical to the single-byte R1 reply.
	; R1 is easily detected by looking at the most significant bit
	; that shall be 0.
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.readanotherbyte:
	call spi_receive_byte ; returns in AL

	and al, al
	jns .got_r1	; Ah ha!
	loop .readanotherbyte

	ERR_TRACE '1'

	; Oups, R1 never came our way. Return with carry set...
	call spi_deselect
	stc
	jmp .done

.got_r1:
	; Check that R1 is not an error
	; and al,al flags still set from above
	jnz .r1_error

	; Now a number of 0xFF should follow, and then there should
	; be a data token (0xFE), followed by a data block of 512 bytes,
	; followed by a 2-byte CRC.

	mov cx, CARD_MAX_BYTES_UNTIL_DATA_TOKEN
.readanotherbyte2:
	call spi_receive_byte ; returns in AL

	cmp al, 0xFE
	je .got_token	; Ah ha!

	; Error token is 000xxxxx format
	test al, 0xE0
	jz .error_token

	loop .readanotherbyte2

	; Oups, token never came our way. Return with carry set...
	ERR_TRACE '2'
	call spi_deselect
	stc
	jmp .done

.error_token:
	ERR_TRACE '3'

	; Oups, there was an error...
	call spi_deselect
	stc
	jmp .done

.r1_error:
	ERR_TRACE '4'

	call spi_deselect
	stc
	jmp .done

.got_token:

	push es
	push di
		mov cx, ds
		mov es, cx
		mov di, bp
%ifdef NO_UNROLL_READ512
		mov cx, 512
		; Now uses ES:DI
		call spi_rxbytes
%else
		call spi_rx512bytes
%endif
	pop di
	pop es

	; Read the CRC (ignoring it for now) and one more byte.
	mov cx, 3*8
	call spi_justclock

	call spi_deselect

	mov cx, 3*8
	call spi_justclock

	clc

.done:


	pop bp
	pop dx
	pop cx
	pop bx
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd18
	;
	; Arguments:	BP to buffer of at least 512 bytes
	;				AX to bits 31-16 of block number
	;				BX to bits 15-0 of block number
	;               CX is the number of blocks to read
	;
	; Return: Writes to buffer, return with the data or error token in AL
	;         (0xFE is the OK value)
	;
	; Returns with carry set on timeout
	;
card_cmd18:
	call spi_select

	push bx
	push cx
	push dx
	push bp


	; Send CMD18
	mov dx, bx	; copy BX to DX, since BX is the argument for spi_send_X

	SPI_SEND_BYTE 0x40 + 18

	mov bx, ax
	call spi_send_word
	mov bx, dx	; Get original BX values
	call spi_send_word

	SPI_SEND_BYTE 0xff	; CRC

	; Before usinc CX for timouts, copy it to BX. It is our block count!
	mov bx, cx

	; The cards sends 0xff until it finally gives the first byte
	; of R7, which is identical to the single-byte R1 reply.
	; R1 is easily detected by looking at the most significant bit
	; that shall be 0.
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.readanotherbyte:
	call spi_receive_byte ; returns in AL

	and al, al
	jns .got_r1	; Ah ha!
	loop .readanotherbyte

	ERR_TRACE '1'

	; Oups, R1 never came our way. Return with carry set...
	call spi_deselect
	stc
	jmp .done

.got_r1:
	; Check that R1 is not an error
	; and al,al flags still set from above
	jnz .r1_error


.next_block:

	; Now a number of 0xFF should follow, and then there should
	; be a data token (0xFE), followed by a data block of 512 bytes,
	; followed by a 2-byte CRC.

	mov cx, CARD_MAX_BYTES_UNTIL_DATA_TOKEN
.readanotherbyte2:
	call spi_receive_byte ; returns in AL

	cmp al, 0xFE
	je .got_token	; Ah ha!

	; Error token is 000xxxxx format
	test al, 0xE0
	jz .error_token

	loop .readanotherbyte2

	; Oups, token never came our way. Return with carry set...
	ERR_TRACE '2'
	call spi_deselect
	stc
	jmp .done

.error_token:
	ERR_TRACE '3'

	; Oups, there was an error...
	call spi_deselect
	stc
	jmp .done

.r1_error:
	ERR_TRACE '4'

	call spi_deselect
	stc
	jmp .done

.got_token:

	push es
	push di
		mov cx, ds
		mov es, cx
		mov di, bp
%ifdef NO_UNROLL_READ512
		mov cx, 512
		; Now uses ES:DI
		call spi_rxbytes
%else
		call spi_rx512bytes
%endif
	pop di
	pop es

	add bp, 512	; Advance buffer by one sector

	; Read the CRC (ignoring it for now)
	mov cx, 2*8
	call spi_justclock

	; Decremenet block count
	dec bx
	jnz .next_block ; Still more to go?

	; No more!
	mov cx, 8
	call spi_justclock


	clc
	call spi_deselect

.done:


	pop bp
	pop dx
	pop cx
	pop bx
.ret:
	ret





	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd23 : Send CMD23 (number of blocks) and check for R1 reply
	;
	; Argument: CX is the number of blocks.
	;
	; AL = R1 value
	; Returns with carry set on timeout
	;
card_cmd23:
	push bx
	push cx
	call spi_select

	SPI_SEND_BYTE 0x40 + 23

	xor bx, bx ; Bits 31-16
	call spi_send_word
	mov bx, cx ; Bits 15-0
	call spi_send_word

	SPI_SEND_BYTE 0xff	; CRC

	; The cards sends 0xff until it finally gives R1. R1
	; is easily detected by looking at the most significant bit
	; that shall be 0.
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.readanotherbyte:
	call spi_receive_byte ; returns in AL

	and al, al
	jns .got_r1	; Ah ha!
	loop .readanotherbyte

	; Oups, R1 never came our way. Return with carry set...
	call spi_deselect
	stc
	jmp .done

.got_r1:
	; Received R1, value is in AL now.

	mov cx, 8
	call spi_justclock

	call spi_deselect
	clc

.done:
	pop cx
	pop bx
	ret





	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd24 : Send CMD24 (Single Block Write) and check for reply
	;
	; Arguments:	ES:SI to buffer of at least 512 bytes
	;				AX to bits 31-16 of block number
	;				BX to bits 15-0 of block number
	;
	; Return: Data Reponse in DL. (00000101) -> Data accepted
	;
	; Returns with carry set on timeout
	;
card_cmd24:
	call spi_select

	push ax
	push bx
	push cx
	push bp

	mov dx, bx	; copy BX to DX, since BX is the argument for spi_send_X

	; Send CMD24
	SPI_SEND_BYTE 0x40 + 24

	mov bx, ax
	call spi_send_word
	mov bx, dx	; Get original BX values
	call spi_send_word

	SPI_SEND_BYTE 0xff	; CRC

	; Wait for R1 response
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.poll_for_r1:
	call spi_receive_byte	; Returns in AL
	and al, al
	jns .got_r1
	loop .poll_for_r1

	ERR_TRACE '7'

	; Oups, R1 never came. Return with carry set..
	mov dl, 0xff		; Return value for R1 timeout
	call spi_deselect
	stc
	jmp .done

.got_r1:

	; Ok, now there must be at least one dummy byte before we send our data packet!
	call spi_receive_byte
	call spi_receive_byte

	; Send start block token
	mov bx, 0xFE
	call spi_send_byte

	; Send our data!
	push ds
	mov bp, si
	mov ax, es
	mov ds, ax
%ifdef NO_UNROLL_WRITE512
	mov cx, 512
	call spi_txbytes	; Source= DS:BP, Length=CX
%else
	call spi_tx512bytes	; Source= DS:BP
%endif
	pop ds

	; Send a fake CRC (TODO : Compute CRC?)
	mov bx, 0x7777
	call spi_send_word

	; Data response and busy polling...
	mov cx, CARD_MAX_BYTES_UNTIL_DATA_RESPONSE_TOKEN
.poll_for_data_token:
	call spi_receive_byte
	; The data response token has the xxx0sss1 format
	;
	; sss is the status. 010 means data accepted.
	;
	; Detect the token by looking at the fixed bits
	mov ah, al	; Keep the token intact in AL
	and ah, 0x11; Isolate the two bits.
	jnz .got_data_token	; Should be 01
	loop .poll_for_data_token

	; Oups, no data token...
	ERR_TRACE '8'
	mov dl, 0x1F	; return value for token timeout
	call spi_deselect
	stc
	jmp .done


.got_data_token:
	and al, 0x1F	; Keep only known bits
	mov dl, al		; and let this be the return value.

	mov cx, CARD_MAX_BYTES_WAIT_IDLE
.wait_idle:
	call spi_receive_byte
	cmp al, 0xff
	je .idle
	loop .wait_idle

	; Oups, card still busy....
	ERR_TRACE '9'
	mov dl, 0x2F		; return value for busy timeout
	call spi_deselect
	stc
	jmp .done

.idle:

	; Read one extra byte to be safe
	call spi_receive_byte
	call spi_deselect
	call spi_receive_byte

	clc	; Clear Carry (no timeout)

.done:

	pop bp
	pop cx
	pop bx
	pop ax

	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd25 : Send CMD25 (Multiple Block Write) and check for reply
	;
	; Arguments:	ES:SI to buffer of at least 512 bytes
	;				AX to bits 31-16 of block number
	;				BX to bits 15-0 of block number
	;               CX number of blocks
	;
	; Return: Data Reponse in DL. (00000101) -> Data accepted
	;
	; Returns with carry set on timeout
	;
card_cmd25:
	call spi_select

	push ax
	push bx
	push cx
	push bp

	mov bp, si

	mov dx, bx	; copy BX to DX, since BX is the argument for spi_send_X

	; Send CMD25
	SPI_SEND_BYTE 0x40 + 25

	mov bx, ax
	call spi_send_word
	mov bx, dx	; Get original BX values
	call spi_send_word

	SPI_SEND_BYTE 0xff	; CRC

	; BX is the block counter
	mov bx, cx

	; Wait for R1 response
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.poll_for_r1:
	call spi_receive_byte	; Returns in AL
	and al, al
	jns .got_r1
	loop .poll_for_r1

	ERR_TRACE '7'

	; Oups, R1 never came. Return with carry set..
	mov dl, 0xff		; Return value for R1 timeout
	call spi_deselect
	stc
	jmp .done

.got_r1:

	; Ok, now there must be at least one dummy byte before we send our data packet!
	call spi_receive_byte
	call spi_receive_byte

.next_block:

	; Send data token
	SPI_SEND_BYTE 0xFC

	; Send our data!
	push ds
	mov ax, es
	mov ds, ax
%ifdef NO_UNROLL_WRITE512
	mov cx, 512
	call spi_txbytes	; Source= DS:BP, Length=CX
%else
	call spi_tx512bytes	; Source= DS:BP
%endif
	pop ds

	; Send a fake CRC (TODO : Compute CRC?)
	;mov bx, 0x7777	; Just use whatever is in BX, we need
	; to preserve BX (block counter)
	call spi_send_word

	; Data response and busy polling...
	mov cx, CARD_MAX_BYTES_UNTIL_DATA_RESPONSE_TOKEN
.poll_for_data_response:
	call spi_receive_byte
	; The data response token has the xxx0sss1 format
	;
	; sss is the status. 010 means data accepted.
	;
	; Detect the token by looking at the fixed bits
	mov ah, al	; Keep the token intact in AL
	and ah, 0x11; Isolate the two bits.
	jnz .got_data_response	; Should be 01
	loop .poll_for_data_response

	; Oups, no data response...
	ERR_TRACE '8'
	mov dl, 0x1F	; return value for token timeout
	call spi_deselect
	stc
	jmp .done


.got_data_response:
	and al, 0x1F	; Keep only known bits
	mov dl, al		; and let this be the return value.

	mov cx, CARD_MAX_BYTES_WAIT_IDLE
.wait_idle:
	call spi_receive_byte
	cmp al, 0xff
	je .idle
	loop .wait_idle

	; Oups, card still busy....
	ERR_TRACE '9'
	mov dl, 0x2F		; return value for busy timeout
	call spi_deselect
	stc
	jmp .done

.idle:

	add bp, 512	; Advance the buffer position by one sector

	; Decrement block count
	dec bx
	jnz .next_block ; More to go?

	; Send a stop transaction token
	SPI_SEND_BYTE 0xFD

	; Read two extra byte to be safe
	call spi_receive_byte
	call spi_receive_byte

	mov cx, CARD_MAX_BYTES_WAIT_IDLE
.final_wait_idle:
	call spi_receive_byte
	cmp al, 0xff
	je .final_idle
	loop .final_wait_idle

	; Infinitely busy?
	ERR_TRACE 'A'
	mov dl, 0x2F		; return value for busy timeout
	call spi_deselect
	stc
	jmp .done


.final_idle:

	clc	; Clear Carry (no timeout)

	call spi_receive_byte
	call spi_deselect
	call spi_receive_byte

.done:

	pop bp
	pop cx
	pop bx
	pop ax

	ret



	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_cmd8 : Send CMD0 and check for R7 reply.
	;
	; Arguments: BP to buffer of at least 6 bytes
	; Return: Writes to buffer, returns R1 value in AL.
	; Returns with carry set on timeout.
	;
card_cmd8:
	call spi_select

	push bx
	push cx
	push bp

	mov cx, 6
	push bp
	mov bp, _dat_cmd8
	call spi_txbytes
	pop bp

	; The cards sends 0xff until it finally gives the first byte
	; of R7, which is identical to the single-byte R1 reply.
	; R1 is easily detected by looking at the most significant bit
	; that shall be 0.
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.readanotherbyte:
	call spi_receive_byte ; returns in AL

	and al, al
	jns .got_r1	; Ah ha!
	loop .readanotherbyte

	; Oups, R1 never came our way. Return with carry set...
	call spi_deselect
	stc
	jmp .done

.got_r1:
	; Received R1, value is in AL now. Save it to the buffer.
	mov [ds:bp], al
	inc bp

	; Command version, reserved bits, voltage accepted, check pattern..
	; Read one extra byte.
	push es
	push di
		mov cx, ds
		mov es, cx
		mov di, bp
		mov cx, 5
		; Now uses ES:DI
		call spi_rxbytes
	pop di
	pop es

	; R1 value still in AL...

	call spi_deselect
	clc

.done:
	pop bp
	pop cx
	pop bx
	ret


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_sendCMD_R1B : Send a command and wait for R1b
	;
	; The B in R1B is for busy. That is, after the R1 response, a number of zeroes
	; will be sent while the card is busy. This function keeps reading until
	; 0xff is received.
	;
	; AL = R1 value
	; BP = Command to send (always 6 bytes)
	; Returns with carry set on timeout
	;
card_sendCMD_R1B:
	call spi_select

	push bx
	push cx
	push bp

	; Transmit command
	mov cx, 6
	call spi_txbytes

	; Stuff byte to be discarded
	call spi_receive_byte

	; The cards sends 0xff until it finally gives R1. R1
	; is easily detected by looking at the most significant bit
	; that shall be 0.
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.readanotherbyte:
	call spi_receive_byte ; returns in AL

	and al, al
	jns .got_r1	; Ah ha!
	loop .readanotherbyte

	ERR_TRACE '%'
	; Oups, R1 never came our way. Return with carry set...
	call spi_deselect
	stc
	jmp .done

.got_r1:
	; Received R1, value is in AL now. Keep a copy in BL for later.
	mov bl, al

	; Read bytes until 0xFF is received
	mov cx, CARD_MAX_BUSY_BYTES
.stillBusy:
	call spi_receive_byte
	cmp al, 0xff
	je .idle
	loop .stillBusy

	ERR_TRACE '&'
	; Busy for ever?
	call spi_deselect
	stc
	jmp .done

.idle:

	mov cx, 8
	call spi_justclock

	call spi_deselect
	clc

.done:
	pop bp
	pop cx
	pop bx
	ret



	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; card_sendCMD_R1 : Send a command and wait for R1
	;
	; AL = R1 value
	; BP = Command to send (always 6 bytes)
	; Returns with carry set on timeout
	;
card_sendCMD_R1:
	call spi_select

	push bx
	push cx
	push bp

	; Transmit command
	mov cx, 6
	call spi_txbytes

	; The cards sends 0xff until it finally gives R1. R1
	; is easily detected by looking at the most significant bit
	; that shall be 0.
	mov cx, CARD_MAX_BYTES_UNTIL_R1
.readanotherbyte:
	call spi_receive_byte ; returns in AL

	and al, al
	jns .got_r1	; Ah ha!
	loop .readanotherbyte

	; Oups, R1 never came our way. Return with carry set...
	call spi_deselect
	stc
	jmp .done

.got_r1:
	; Received R1, value is in AL now.

	mov cx, 8
	call spi_justclock

	call spi_deselect
	clc

.done:
	pop bp
	pop cx
	pop bx
	ret


section .data

_dat_cmd0: db 0x40, 0, 0, 0, 0x0, 0x95
_dat_cmd1: db 0x41, 0, 0, 0, 0x0, 0xF7
_dat_cmd8: db 0x40+8, 0, 0, 1, 0xAA, 0x87
_dat_cmd9: db 0x40+9, 0, 0, 0, 0x00, 0xAF
_dat_cmd10: db 0x40+10, 0, 0, 0, 0x0, 0x1B
_dat_cmd12: db 0x40+12, 0, 0, 0, 0, 0xFF ; CRC todo
_dat_cmd16: db 0x40+16, 0, 0, 0x02, 0, 0xFF ; CRC todo
_dat_cmd55: db 0x40+55, 0, 0, 0, 0, 0xF7
_dat_cmd59: db 0x40+59, 0, 0, 0, 0, 0x91
_dat_amcd41: db 0x40+41, 0x40, 0, 0, 0, 0xE5
