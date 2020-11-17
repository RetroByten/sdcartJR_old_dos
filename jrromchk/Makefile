NASM=nasm
CC=gcc
LD=$(CC)
include version.inc
CFLAGS=-Wall -O0 -DVERSION=\"$(VERSION)\"

all: jrromchk

jrromchk: jrromchk.o
	$(LD) $(LDFLAGS) $^ -o $@

clean:
	rm -f *.o jrromchk
