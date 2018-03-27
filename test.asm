	* = $8000
	.enc "ascii"
	.cdef $0,$7f,$0
	.edef "\n", $0a

reg_putc	= $6000
reg_putnum	= $6001

mult0		= $10
mult1		= $11

main
	#mul_test $00, $00
	#mul_test $04, $04
	#mul_test $10, $10
	#mul_test $ff, $ff
	brk

writev	.macro
	vals = [\@]
	.for i := 0, i < len(vals), i += 2
		.if i < 2 || vals[i-2] != vals[i]
			lda vals[i]
		.endif
		sta vals[i+1]
	.next
	.endm

puts	.macro s
	strlen = len(\s)
	.cerror strlen > $ff, "string too large"
	.if strlen > 0
	;; start x at a higher place so that it wraps to 0, eliminating a
	;; comparison
	x_start = $100 - strlen
	ldx #x_start
	bne _print
string	.text \s
_print
	lda string - x_start, x
	sta reg_putc
	inx
	bne _print
	.endif
	.endm

mul_test .macro v0, v1
	.puts format("multiply %02x and %02x, expects %04x: ", \v0, \v1, \v0*\v1)
	#writev #\v0, mult0, #\v1, mult1
	jsr mult
	#writev mult1, reg_putnum, mult0, reg_putnum
	#writev #"\n", reg_putc
	.endm

mult	.proc
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
	.pend

	* = $fffa
	.addr main, 0, 0
