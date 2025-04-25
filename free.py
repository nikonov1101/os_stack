MBR_END = 509 # whole mbr without magic bytes
BOOT1_end = 512 + 509 # mbr + one more sector w/o magic.

with open("bootloader.bin", "rb") as f:
    bs = f.read()
    i = MBR_END
    while i> 0:
        if bs[i] != 0:
            break
        i -= 1
    print(f"{MBR_END - i} free bytes in MBR sector" )

    i = BOOT1_end
    while i> 0:
        if bs[i] != 0:
            break
        i -= 1
    print(f"{BOOT1_end - i} free bytes in BOOT1 sector" )

