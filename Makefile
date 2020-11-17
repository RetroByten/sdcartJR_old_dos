all: bios driver tools

.PHONY: bios driver tools

bios:
	$(MAKE) -C bios

driver:
	$(MAKE) -C driver

tools:
	$(MAKE) -C tools

clean:
	$(MAKE) -C tools clean
	$(MAKE) -C driver clean
	$(MAKE) -C bios clean
	rm outputs/* -f

