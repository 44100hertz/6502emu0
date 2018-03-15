.org $8000

REG_PUTC = $6000

text:
  .db 'hello, world', $0a, 0
main:
  ldx #$ff
print_hello:
  inx
  lda text, x
  sta REG_PUTC
  bne print_hello

.org $fffa
  dw main
  dw main
  dw main
