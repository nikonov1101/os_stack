default: clean os
	qemu-system-i386 -m 256m -boot c -hda boot.img

gdb: clean os
	qemu-system-i386 -s -S -m 256m -boot c -hda boot.img

os: bootloader early32
	cat bootloader.bin early32.bin > boot.img
	@ls -l boot.img

s:
	cc -std=c11 -pedantic -g3 -Wall -Wextra -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=undefined -o scratch ./scratch.c && ./scratch

KERNEL_BLOCKS ?= $(shell python3 -c 'import os; import math as m; print(m.ceil(os.stat("early32.bin").st_size/512))')
EARLY_TEXT_AT ?= 0x8000

bootloader: early32
	KERNEL_BLOCKS=$(KERNEL_BLOCKS) \
	nasm -f bin -o bootloader.bin bootloader.asm
	@python3 ./free.py

bootloader/symbols: bootloader
	cat boot1.asm | grep -v '\[ORG' > tmp.asm
	KERNEL_BLOCKS=$(KERNEL_BLOCKS) nasm -f elf -Fdwarf tmp.asm -o tmp.elf
	objdump -t tmp.elf > symbols.table
	rm -f tmp.asm tmp.elf
	python3 table.py
	@rm symbols.table

early32: libk
	@i386-elf-gcc  -g -ffreestanding -c early32.c -o early32.o
	@i386-elf-ld -Map=early32.map -T linker.ld -o early32.bin libk.o early32.o --oformat binary
	@i386-elf-ld -T linker.ld -o early32.elf libk.o early32.o

.PHONY: libk
libk:
	i386-elf-gcc -Wno-builtin-declaration-mismatch -g -c libk/*.c -o libk.o

clean:
	@rm -f *.elf *.o *.bin *.img
