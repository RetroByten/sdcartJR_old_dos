NASM=nasm -I ../common
JRROMCHK=../jrromchk/jrromchk
VERSION=0.30
OUTDIR=../outputs

VPATH=../common

includes=spi.asm card_io.asm card_cmd.asm strutil_bios.asm int13h.asm int19h.asm chs2lba.asm chsfitter.asm print16.asm memory.asm

all: $(OUTDIR)/sdcart.rom

main.tmp: main.asm $(includes)
	$(NASM) $< -fbin -o $@ -l main.lst -DVERSION_STR=\"$(VERSION)\"

$(OUTDIR)/sdcart.rom: main.tmp $(JRROMCHK)
	$(JRROMCHK) $< -s -p -o $@ -v

../jrromchk/jrromchk:
	$(MAKE) -C ../jrromchk

clean:
	$(MAKE) -C ../jrromchk clean
	rm -f *.map *.lst *.jrc *.bin *.rom main.tmp
