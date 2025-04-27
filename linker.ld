OUTPUT_FORMAT("binary")
OUTPUT_ARCH("i386")

SECTIONS
{
    . = 0x8000;
    .early32 : { *(.early32) }
    .text : { *(.text) }
    .rodata : { *(.rodata) }
    .data : { *(.data) }
    .bss : { *(.bss) }
}
