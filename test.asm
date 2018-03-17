org $8000

REG_PUTC = $6000
STRING_PTR = $0

text:
        .db 'hello, world', $0a, 0
main:
        lda #text & $ff
        sta STRING_PTR
        lda #text >> 8
        sta STRING_PTR+1
        jsr print_string

        brk

print_string:
        ldy #0
        jmp readchar
putchar:
        iny
        sta REG_PUTC
readchar:
        lda (STRING_PTR),y
        bne putchar
        rts

org $fffa
        dw main
        dw 0
        dw 0
