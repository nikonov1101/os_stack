default: clean os
	qemu-system-i386 -m 256m -boot c -hda boot.img

gdb: clean os
	qemu-system-i386 -s -S -m 256m -boot c -hda boot.img

os: bootloader early32
	cat bootloader.bin early32.bin > boot.img
	@ls -l boot.img

s:
	cc -std=c11 -pedantic -g3 -Wall -Wextra -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=undefined -o scratch ./scratch.c && ./scratch

KERNEL_BLOCKS := $(shell wc -c early32.bin | awk '{print int(0.5+$$1/512)}')
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

early32:
	@i386-elf-gcc -g -ffreestanding -c early32.c -o early32.o
	@i386-elf-ld -T linker.ld -o early32.bin early32.o --oformat binary
	@i386-elf-ld -T linker.ld -o early32.elf early32.o

clean:
	@rm -f *.o *.bin *.img
