default: bootloader
	qemu-system-x86_64 -boot a -fda boot.img

bootloader:
	nasm boot1.asm -f bin -o boot.img

