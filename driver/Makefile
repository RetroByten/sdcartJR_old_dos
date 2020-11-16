NASM=nasm -I../common

OUTPUTS=../outputs
VPATH=../common

all: sdcart.sys

sdcart.sys: sdcart.asm strutil.asm spi.asm card_io.asm card_cmd.asm
	$(NASM) $< -fbin -o $@
	cp $@ $(OUTPUTS)

clean:
	rm -f *.sys *.map *.lst

