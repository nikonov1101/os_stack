default: bootloader
	qemu-system-x86_64 -m 99m -boot a -fda boot.img

gdb: bootloader
	qemu-system-x86_64 -s -S -m 256m -boot a -fda boot.img

symbols:
	cat boot1.asm | grep -v '\[ORG' > tmp.asm
	nasm -f elf -Fdwarf tmp.asm -o tmp.elf
	objdump -t tmp.elf > symbols.table
	rm -f tmp.asm tmp.elf
	python3 table.py

bootloader:
	nasm boot1.asm -f bin -o boot.img

