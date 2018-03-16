org $8000

REG_PUTC = $6000

text:
  .db 'hello, world', $0a, 0
main:
  ldx #0
  jmp readchar
putchar:
  inx
  sta REG_PUTC
readchar:
  lda text, x
  bne putchar

  brk

.org $fffa
  dw main
  dw main
  dw main
