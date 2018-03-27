	* = $8000
	.enc "ascii"
	.cdef $0,$7f,$0
	.edef "\n", $0a

reg_putc	:= $6000
reg_putnum	:= $6001

string_ptr	:= $0
mult0		:= $10
mult1		:= $11

main
	.mul_test $00, $00
	.mul_test $04, $04
	.mul_test $10, $10
	.mul_test $ff, $ff
	brk

put16	.macro val, into
	lda <\val
	sta \into
	lda >\val
	sta \into+1
	.endm

mul_test .macro v0, v1
	jmp start
	;; info
info	.null format("multiply %02x and %02x, expects %04x: ", \v0, \v1, \v0*\v1)
start
	.put16 #info, string_ptr
	jsr print_string
	;; multiply
	lda #\v0
	sta mult0
	lda #\v1
	sta mult1
	jsr mult
	;; print result
	lda mult1
	sta reg_putnum
	lda mult0
	sta reg_putnum
	lda #"\n"
	sta reg_putc
	.endm

mult
	lda #0
	ldx #8
	lsr mult0
_loop
	bcc _noadd
	clc
	adc mult1
_noadd
	ror a
	ror mult0
	dex
	bne _loop
	sta mult1

	rts

print_string
	ldy #0
_putc
 	lda (string_ptr),y
	beq _done
	sta reg_putc
	iny
	beq _done
	bne _putc
_done
	rts

	* = $fffa
	.addr main, 0, 0
