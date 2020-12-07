NASM=nasm -I../common

OUTPUTS=../outputs
VPATH=../common

all: sdcart.sys sdcartl.sys
	ls -l *.sys

sdcart.sys: sdcart.asm strutil.asm spi.asm card_io.asm card_cmd.asm
	$(NASM) $< -fbin -o $@
	cp $@ $(OUTPUTS)

sdcartl.sys: sdcart.asm strutil.asm spi.asm card_io.asm card_cmd.asm
	$(NASM) $< -fbin -o $@ -DLOWMEM
	cp $@ $(OUTPUTS)

clean:
	rm -f *.sys *.map *.lst

