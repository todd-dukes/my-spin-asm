
tests:
	../src/my-spin-asm < mnemonics.spn > mnemonics.hex
	unix2dos mnemonics.hex
	diff mnemonics.hex mnemonics-known-good.hex

clean:
	rm -f mnemonics.hex
