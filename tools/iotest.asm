bits 16
cpu 8086
org 100h

; This must come first, before other code or included code!
entry:
	jmp main

%include 'strutil.asm'

section .data

test_filename: db "IOTEST.BIN",0

section .bss

%define NUM_BLOCKS		10	; Test will be done using 100k
%define READBUF_SIZE	(10*1024)	; 10 kB

handle: resw 1
writebuf: resb READBUF_SIZE
blocks: resw 1

write_start_clock: resw 2
write_end_clock: resw 2
read_start_clock: resw 2
read_end_clock: resw 2

elapsed_write: resw 2
elapsed_read: resw 2

section .text

main:
	printStringLn "Simple File I/O Test."

	;;;; Phase 1 : Writing ;;;;

	; Create file
	mov ax, 3C00h
	mov cx, 0		; Attributes
	mov dx, test_filename
	int 21h
	jc .create_file_error
	mov [handle], ax

	printStringLn "Write test..."

	; Read system clock
	mov ax, 0
	int 1Ah
	mov [write_start_clock], dx
	mov [write_start_clock+2], cx

	; Write file...
	mov word [blocks], NUM_BLOCKS
.loop:
	mov ax, 4000h
	mov bx, [handle]
	mov cx, READBUF_SIZE
	mov dx, writebuf
	int 21h
	jc .write_error

	dec word [blocks]
	jnz .loop

	; Close file, also flushes data to disk I hope...
	call closeFile

	; ...but just in case, perform a disk reset too. This will also make sure
	; data is read again from disk during the read test that follows.
	mov ax, 0D00h
	int 21h

	; Now writing is all done, read system clock again to record the time
	mov ax, 0
	int 1Ah
	mov [write_end_clock], dx
	mov [write_end_clock+2], cx


	;;;; Phase 2 : Reading ;;;;

	; Open file
	mov ax, 3D00h	; Open file, read-only
	mov dx, test_filename
	int 21h
	jc .open_error
	mov [handle], ax

	; record start time
	mov ax, 0
	int 1Ah
	mov [read_start_clock], dx
	mov [read_start_clock+2], cx

	printStringLn "Read test..."

	; Read...
	mov word [blocks], NUM_BLOCKS
.loop2:
	mov ax, 3F00h
	mov bx, [handle]
	mov cx, READBUF_SIZE
	mov dx, writebuf
	int 21h
	dec word [blocks]
	jnz .loop2

	; record stop time
	mov ax, 0
	int 1Ah
	mov [read_end_clock], dx
	mov [read_end_clock+2], cx


	call closeFile


.summary:
	printStringLn "Done."
	call newline

	printString "Write start: "
	mov dx, [write_start_clock+2]
	call printHexWord
	mov dx, [write_start_clock]
	call printHexWord
	call newline

	printString "Write end: "
	mov dx, [write_end_clock+2]
	call printHexWord
	mov dx, [write_end_clock]
	call printHexWord
	call newline

	printString "Read start: "
	mov dx, [read_start_clock+2]
	call printHexWord
	mov dx, [read_start_clock]
	call printHexWord
	call newline

	printString "Read end: "
	mov dx, [read_end_clock+2]
	call printHexWord
	mov dx, [read_end_clock]
	call printHexWord
	call newline

	; Compute and display time ticks from writing
	mov ax, [write_end_clock]
	mov bx, [write_start_clock]
	sub ax, bx
	mov [elapsed_write], ax
	mov ax, [write_end_clock+2]
	mov bx, [write_start_clock+2]
	sbb ax, bx
	mov [elapsed_write+2], ax
	printString "Write ticks: "
	mov dx, [elapsed_write+2]
	call printHexWord
	mov dx, [elapsed_write]
	call printHexWord
	call newline

	; Compute and display time ticks from reading
	mov ax, [read_end_clock]
	mov bx, [read_start_clock]
	sub ax, bx
	mov [elapsed_read], ax
	mov ax, [read_end_clock+2]
	mov bx, [read_start_clock+2]
	sbb ax, bx
	mov [elapsed_read+2], ax
	printString "Read ticks: "
	mov dx, [elapsed_read+2]
	call printHexWord
	mov dx, [elapsed_read]
	call printHexWord
	call newline



	jmp exit

.write_error:
	printStringLn "Error writing to file"
	call closeFile
	jmp exit

.create_file_error:
	printStringLn "Could not create file"
	jmp exit

.open_error:
	printStringLn "Could not open file"
	jmp exit

exit:
	mov ax, 4C00h
	int 21h


closeFile:
	push ax
	push bx
	mov ax, 3E00h
	mov bx, [handle]
	int 21h
	pop bx
	pop ax
	ret

