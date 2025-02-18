default: bootloader
	qemu-system-i386 -m 256m -boot c -hda boot.img

gdb: bootloader
	qemu-system-i386 -s -S -m 256m -boot c -hda boot.img

bootloader:
	nasm boot1.asm -f bin -o boot.img

symbols:
	cat boot1.asm | grep -v '\[ORG' > tmp.asm
	nasm -f elf -Fdwarf tmp.asm -o tmp.elf
	objdump -t tmp.elf > symbols.table
	rm -f tmp.asm tmp.elf
	python3 table.py
	@rm symbols.table

