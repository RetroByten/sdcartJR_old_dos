NASM=nasm -I../common
OUTDIR=../outputs
VPATH=../common

all: sy.com iotest.com sdpart.com
	cp $^ $(OUTDIR)

sy.com: sy.asm
	$(NASM) $< -fbin -o $@

iotest.com: iotest.asm
	$(NASM) $< -fbin -o $@

sdpart.com: sdpart.asm chs2lba.asm strutil.asm itoa.asm hexdump.asm card_io.asm card_cmd.asm spi.asm
	$(NASM) $< -fbin -o $@

clean:
	rm *.com *.lst *.map -f
