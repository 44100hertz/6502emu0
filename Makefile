run: build

build: $(patsubst %.asm,%.bin,$(wildcard *.asm))
	cargo run

%.bin: %.asm
	64tass --ascii --case-sensitive --nostart --m6502 --long-branch -Wall -Woptimize -Wstrict-bool -Wunused $< -o $@
