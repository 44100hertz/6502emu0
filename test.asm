* = $8000

	.enc "ascii"
	.cdef $0,$7f,$0
	.edef "\n", $0a

reg_putc	:= $6000
reg_putnum	:= $6001

string_ptr	:= $0
mult0		:= $10
mult1		:= $11


put16	.macro val, into
	lda #<\val
	sta \into
	lda #>\val
	sta \into+1
	.endm
main

mul_test
	jmp _start
info	.null "Multiply f0 * 11, expects 0f f0: "
_start
	.put16 info, string_ptr
	jsr print_string
	lda #$f0
	sta mult0
	lda #$11
	sta mult1
	;; multiply
	jsr mult
	;; print result
	lda mult1
	sta reg_putnum
	lda mult0
	sta reg_putnum
done
	brk

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
