;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File sy.asm
;;;
;;; Source code for sy.com, a simple tool to perform a disk reset.
;;; This is done using DOS Int 21h function 0Dh, which flushes all
;;; file buffers to disk according. It also appears to make DOS forget
;;; any sectors that are still buffered in memory.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bits 16
cpu 8086
org 100h

entry:
	mov ax, 0D00h	; Disk Reset
	int 21h

	mov ax, 4C00h	; Exit
	int 21h
