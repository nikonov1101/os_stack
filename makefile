default: clean os
	qemu-system-i386 -m 256m -boot c -hda boot.img

gdb: clean os
	qemu-system-i386 -s -S -m 256m -boot c -hda boot.img

os: bootloader k_early
	cat bootloader.bin early.bin > boot.img
	@ls -l boot.img

s:
	cc -std=c11 -pedantic -g3 -Wall -Wextra -Wconversion -Wdouble-promotion -Wno-unused-parameter -Wno-unused-function -Wno-sign-conversion -fsanitize=undefined -o scratch ./scratch.c && ./scratch

EARLY_TEXT_AT ?= 0x8000
KERNEL_ENTRYPOINT := 0x$(shell objdump --adjust-vma=$(EARLY_TEXT_AT) --section=.text -t early.o | grep k_early | cut -d' ' -f1)

bootloader: k_early
	KERNEL_START_ADDR=$(KERNEL_ENTRYPOINT) nasm -f bin -o bootloader.bin boot1.asm
	@python3 ./free.py

symbols: bootloader
	cat boot1.asm | grep -v '\[ORG' > tmp.asm
	nasm -f elf -Fdwarf tmp.asm -o tmp.elf
	objdump -t tmp.elf > symbols.table
	rm -f tmp.asm tmp.elf
	python3 table.py
	@rm symbols.table

k_early:
	@i386-elf-gcc -g -ffreestanding -c early.c -o early.o
	@nasm early.asm -f elf -o early_loader.o
	@i386-elf-ld -o early.bin -Ttext $(EARLY_TEXT_AT) early.o early_loader.o --oformat binary
	@i386-elf-ld -o early.elf -Ttext $(EARLY_TEXT_AT) early.o early_loader.o


clean:
	@rm -f *.o *.bin *.img
