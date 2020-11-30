RELEASE_FILE=sdcjr02.zip

all: bios driver tools

.PHONY: bios driver tools release

bios:
	$(MAKE) -C bios

driver:
	$(MAKE) -C driver

tools:
	$(MAKE) -C tools

release: all
	zip -jr $(RELEASE_FILE) release/*
	zip -jr $(RELEASE_FILE) outputs/*


clean:
	$(MAKE) -C tools clean
	$(MAKE) -C driver clean
	$(MAKE) -C bios clean
	rm outputs/* release/*.com release/*.sys release/*.rom -f

