# SD-Cart JR : MMC/SD card reader cartridge for PCjr

## Overview

This repository contains the source code for the tools, boot ROM and device
driver for use with the SD-Cart JR cartridge.

For more information about this cartridge and how to use it, please visit the project page.

* [Project page (english)](https://www.raphnet.net/electronique/sdcartJR/index_en.php)
* [Project page (french)](https://www.raphnet.net/electronique/sdcartJR/index.php)

## Compilation

On a Linux or Unix system, with make, nasm and gcc in your path, it should be as simple
as typing make in the base directory. YMMV.

## Contents

### The sdcart.sys device driver

Scans the card to find the first partition and renders it available as a DOS drive.
Installation is a simple matter of adding DEVICE=SDCART.SYS to config.sys.

If you are willing to sacrifice a few kB/s of transfer speed to save memory, SDCARTL.SYS
is also available. (It uses approx. 3kB in size instead of 9kB).

Starting with version 0.5, when used on DOS 3.0+, sdcart.sys supports two command-line switches:

 * /Y  Instead of asking, always install (Yes)
 * /A  Install only if a card is detected (Auto)

For intance, to always install, use this in your config.sys file:

DEVICE=SDCART.SYS /Y

### Boot ROM

Boot ROM for PCjr which replaces int 13h (The BIOS disk services) to make it possible to use a card for booting as if it were a hard drive. When using this ROM, sdcard.sys is not necessary, DOS supports the card as a hard drive. Fdisk and format can even be used normally.


### sdpart.com

The sdpart.com tool can display and partly decode the contents of the card's CSD and CID registers.
It also shows how a card is partitioned.

The name "sdpart" implies that this should be a full partitioning tool for memory cards. In the long run it may become so, but right now, it is extremely limited. Still, it is a good tool to test if a card is compatible or it may help understand why things are not working.


## Authors

* **Raphael Assenat**


## License

GPLv2. See LICENSE for more information.
